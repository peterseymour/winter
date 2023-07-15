#include <vector>

struct Block {
    uint32_t ip_cont;
    uint32_t xp_cont;
    block_type_t type;
    uint32_t valstack_height;

    struct Ref {
        bool is_break;
        uint32_t base_ip;
        uint32_t base_xp;
        uint32_t block_id;
        uint32_t ndrop;
        uint32_t nkeep;
    };
};

const byte_t UNREACHABLE    = 0x00;

const byte_t BLOCK          = 0x02; //+1
const byte_t LOOP           = 0x03; //+1
const byte_t IF             = 0x04; //+1 (nvals=-1)
const byte_t ELSE           = 0x05; //+1/-1
const byte_t END            = 0x0b; //-1

const byte_t BR             = 0x0c;
const byte_t BR_IF          = 0x0d; //(nvals=-1)
const byte_t BR_TABLE       = 0x0e; //(nvals=-1)

const byte_t RETURN         = 0x0f;
const byte_t CALL           = 0x10;
const byte_t CALL_INDIRECT  = 0x11;
const byte_t RETURN_CALL    = 0x12;

static constexpr auto not_defined = std::numeric_limits<uint32_t>::max();

void WASMFunction::build_auxillary() {
    if (auxs != nullptr)
        return;

    Instance* instance = owning.instance;

    const auto is_forward = [&](uint32_t i) -> bool {return i == not_defined;};

    IF_TRACING(std::cout << "\n-- Building " << funcidx << " --" << std::endl;)

    Reader rdr(body->instructions.begin, body->instructions.end);

    std::vector<Block> blocks;
    std::vector<Block::Ref> refs;
    std::vector<uint32_t> stack;
    const uint32_t frame_size = (uint32_t) signature->params.size + nlocals;
    uint32_t valstack_height = 0;


    const auto new_block = [&](const block_type_t& type, bool is_loop=false) {
        stack.push_back(blocks.size());
        blocks.push_back(Block{
            .ip_cont = is_loop ? (uint32_t) rdr.relpos() : not_defined,
            .xp_cont = is_loop ? (uint32_t) refs.size() : not_defined,
            .type = type,
            .valstack_height = valstack_height,
        });
    };

    const auto& end_block = [&](uint32_t delta=0) -> const auto& {
        check(stack.size() > 0, "Block stack underflow");

        const auto block_id = stack.back();
        auto& block = blocks[block_id];

        check(valstack_height == block.valstack_height + block.type.nvals(), "block ended with wrong value stack height");

        if (is_forward(block.ip_cont)) {
            block.ip_cont = block_id == 0 ? rdr.relpos() - 1 : rdr.relpos(); //don't go one past 'end' for top level block
            block.xp_cont = refs.size() + delta;
        }
        stack.pop_back();

        return block;
    };

    const auto new_ref = [&]() {
        refs.push_back(Block::Ref{
            .is_break = false,
            .base_ip = (uint32_t) rdr.relpos(),
            .base_xp = (uint32_t) refs.size(),
            .block_id = stack.back(),
            .ndrop = 0,
            .nkeep = 0,
        });
    };

    const auto break_ref = [&](uint32_t base_ip, uint32_t base_xp, uint32_t depth) {
        const auto block_id = stack[stack.size() - depth - 1];
        const auto& block = blocks[block_id];

        const auto target_height = is_forward(block.ip_cont) ? block.valstack_height + block.type.nvals() : block.valstack_height;

        check(valstack_height >= target_height, "Invalid valstack height for break calculation");

        refs.push_back(Block::Ref{
            .is_break = true,
            .base_ip = base_ip,
            .base_xp = base_xp,
            .block_id = block_id,
            .ndrop = valstack_height - target_height,
            .nkeep = is_forward(block.ip_cont) ? block.type.nrets : block.type.nargs,
        });
    };

    const auto return_ref = [&](uint32_t nkeep) {
        const auto target_height = nkeep;

        refs.push_back(Block::Ref{
            .is_break = false,
            .base_ip = (uint32_t) rdr.relpos(),
            .base_xp = (uint32_t) refs.size(),
            .block_id = 0,
            .ndrop = frame_size + (valstack_height - target_height),
            .nkeep = nkeep,
        });
    };

    const auto unreachable = [&]() {
        auto begin = rdr.relpos();
        for (uint32_t d = 0; d > 0 or (rdr.peek<byte_t>() != ELSE and rdr.peek<byte_t>() != END); d += ::read<instruction_t>(rdr).depth)
            ;
        auto end = rdr.relpos();

        const auto& block = blocks[stack.back()];

        valstack_height = block.valstack_height + block.type.nvals();

        IF_TRACING(
            if (end > begin) {
                std::cout << "in[" << std::setw(4) << begin << "-" << std::setw(4) << end-1 << "]: ";
                for (auto i = 0; i < stack.size(); ++i)
                    std::cout << "  ";
                std::cout << "<unreachable code>" << std::endl;
            }
        )
    };

    IF_TRACE_SWITCH(
        auto indent = [&](bool with_block_id=false, int extra=0) {
            std::cout << "in[" << std::dec << std::setw(4) << rdr.relpos();
            if (with_block_id)
                std::cout << "/" << std::setw(4) << refs.size();
            else
                std::cout << "     ";
            std::cout << "]: ";

            for (auto i=0; i < stack.size() + extra; ++i)
                std::cout << "  ";

            return "";
        };

        auto nvals_info = [&](int32_t nvals=-1000) {
            std::cout << " {";
            if (nvals != -1000)
                std::cout << (nvals > 0 ? "+" : "") << std::dec << nvals << " ";
            std::cout << "@" << valstack_height << "}";
            return "";
        };
    )

    IF_TRACING(std::cout << "function(" << *signature << ")[" << nlocals << "]" << std::endl;)

    new_block(block_type_t{
        .nargs = 0, //params are part of the frame not the stack
        .nrets = (uint32_t) signature->returns.size,
    });

    while (stack.size() > 0) {
        switch (rdr.peek<byte_t>()) {
            case UNREACHABLE: {
                IF_TRACING(std::cout << indent() << "unreachable";)

                ::read<byte_t>(rdr);

                IF_TRACING(std::cout << nvals_info() << std::endl;)

                unreachable();
                break;
            }
            case BLOCK: {
                IF_TRACING(std::cout << indent() << "block";)

                ::read<byte_t>(rdr);

                const auto bt = ::read<block_signature_type_t>(rdr);

                new_block(instance->resolve_block_signature_type(bt));

                IF_TRACING(std::cout << "(" << instance->resolve_block_signature_type(bt) << ")" << std::endl;)
                break;
            }
            case LOOP: {
                IF_TRACING(std::cout << indent() << "loop";)

                ::read<byte_t>(rdr);

                const auto bt = ::read<block_signature_type_t>(rdr);

                new_block(instance->resolve_block_signature_type(bt), true);

                IF_TRACING(std::cout << "(" << instance->resolve_block_signature_type(bt) << ")" << std::endl;)
                break;
            }
            case IF: {
                IF_TRACING(std::cout << indent(true) << "if";)

                ::read<byte_t>(rdr);

                const auto bt = ::read<block_signature_type_t>(rdr);

                valstack_height -= 1;

                new_block(instance->resolve_block_signature_type(bt));
                new_ref();

                IF_TRACING(std::cout << "(" << instance->resolve_block_signature_type(bt) << ")" << nvals_info(-1) << std::endl;)
                break;
            }
            case ELSE: {
                ::read<byte_t>(rdr);

                const auto& block = end_block(+1);

                IF_TRACING(std::cout << indent(true) << "else";)

                valstack_height = block.valstack_height;

                new_block(block.type);
                new_ref();

                IF_TRACING(std::cout << nvals_info() << std::endl;)
                break;
            }
            case END: {
                IF_TRACING(std::cout << indent() << "end";)

                ::read<byte_t>(rdr);

                end_block();

                IF_TRACING(std::cout << nvals_info() << std::endl;)
                break;
            }
            case BR: {
                IF_TRACING(std::cout << indent(true) << "br ";)

                ::read<byte_t>(rdr);

                const auto depth = ::read<varuint32_t>(rdr);

                break_ref((uint32_t) rdr.relpos(), (uint32_t) refs.size(), depth);

                IF_TRACING(std::cout << depth << nvals_info() << std::endl;)
                unreachable();
                break;
            }
            case BR_IF: {
                IF_TRACING(std::cout << indent(true) << "br_if ";)

                ::read<byte_t>(rdr);

                const auto depth = ::read<varuint32_t>(rdr);

                valstack_height -= 1;

                break_ref((uint32_t) rdr.relpos(), (uint32_t) refs.size(), depth);

                IF_TRACING(std::cout << depth << nvals_info(-1) << std::endl;)
                break;
            }
            case BR_TABLE: {
                IF_TRACING(std::cout << indent() << "br_table ";)

                ::read<byte_t>(rdr);

                const auto length = ::read<varuint32_t>(rdr);

                const auto base_ip = rdr.relpos();
                const auto base_xp = refs.size();

                IF_TRACING(std::cout << length << std::endl;)

                valstack_height -= 1;

                for (auto i = 0; i < length; ++i) {
                    IF_TRACING(std::cout << indent(true, 1);)

                    const auto depth = ::read<varuint32_t>(rdr);

                    IF_TRACING(std::cout << depth << std::endl;)

                    break_ref((uint32_t) base_ip, (uint32_t) base_xp, depth);
                }

                IF_TRACING(std::cout << indent(true, 1);)

                const auto depth = ::read<varuint32_t>(rdr);

                IF_TRACING(std::cout << depth << std::endl;)

                break_ref((uint32_t) base_ip, (uint32_t) base_xp, depth);

                unreachable();
                break;
            }
            case RETURN: {
                IF_TRACING(std::cout << indent(true) << "return";)

                ::read<byte_t>(rdr);

                return_ref(signature->returns.size);

                IF_TRACING(std::cout << nvals_info() << std::endl;)
                unreachable();
                break;
            }
            case CALL: {
                IF_TRACING(std::cout << indent() << "call";)

                ::read<byte_t>(rdr);

                const auto funcidx = ::read<varuint32_t>(rdr);
                const auto fn = instance->function_space[funcidx];
                const int32_t nvals = (int32_t) fn->signature->returns.size - (int32_t) fn->signature->params.size;

                valstack_height += nvals;

                IF_TRACING(std::cout << "(" << funcidx << ": " << *fn->signature << ")" << nvals_info(nvals) << std::endl;)
                break;
            }
            case CALL_INDIRECT: {
                IF_TRACING(std::cout << indent() << "call_indirect";)

                ::read<byte_t>(rdr);

                const auto typeidx = ::read<varuint32_t>(rdr);
                const auto tableidx = ::read<varuint32_t>(rdr);

                const auto sig = instance->module->type_section->_.elems[typeidx];
                check(sig->form == signature_type_t::FUNC, "Invalid call indirect signature");
                const auto fn_sig = &sig->func;

                const int32_t nvals = (int32_t) fn_sig->returns.size - (int32_t) fn_sig->params.size;

                valstack_height -= 1;
                valstack_height += nvals;

                IF_TRACING(std::cout << "(*:" << *fn_sig << ")" << nvals_info(-1 + nvals) << std::endl;)
                break;
            }
            case RETURN_CALL: {
                IF_TRACING(std::cout << indent(true) << "return_call";)

                ::read<byte_t>(rdr);

                const auto funcidx = ::read<varuint32_t>(rdr);
                const auto fn = instance->function_space[funcidx];

                check(signature->returns == fn->signature->returns, "tail has differing return type to parent call");

                return_ref(fn->signature->params.size);

                valstack_height = fn->signature->returns.size;

                IF_TRACING(std::cout << "(" << funcidx << ": " << *fn->signature << ")" << nvals_info() << std::endl;)

                unreachable();
                break;
            }
            default: {
                IF_TRACING(std::cout << indent() << std::hex << (int) rdr.peek<byte_t>() << std::dec;)

                const auto instr = ::read<instruction_t>(rdr);

                check(instr.nvals != instruction_t::var_nvals, "Expected fixed nvals");
                valstack_height += instr.nvals;

                IF_TRACING(std::cout << nvals_info(instr.nvals) << std::endl;)
                break;
            }
        }
    }

    check(stack.size() == 0, "Block stack malformed at end of function");
    check(valstack_height == signature->returns.size, "Value stack height mismatch with function return size");
    check(blocks[0].type.nargs == 0, "Initial block has wrong arg count");
    check(blocks[0].type.nrets == (uint32_t) signature->returns.size, "Initial block has wrong returns count");

    //Patch final end as a return
    check(body->instructions.end[-1] == END, "Final function instruction not 'end'");
    body->instructions.end[-1] = RETURN;

    return_ref(signature->returns.size);

    auto auxs = new aux_t[refs.size()];

    for (auto i = 0; i < refs.size(); ++i) {
        const auto& ref = refs[i];

        const auto* block = &blocks[ref.block_id];

        //break over top of else block
        if (ref.is_break and body->instructions.begin[block->ip_cont-1]== ELSE)
            block = &blocks[refs[block->xp_cont-1].block_id];

        auxs[i].ip_offset = (int32_t) block->ip_cont - (int32_t) ref.base_ip;
        auxs[i].xp_offset = (int32_t) block->xp_cont - (int32_t) ref.base_xp;
        auxs[i].nkeep = ref.nkeep;
        auxs[i].ndrop = ref.ndrop;
    }

    IF_TRACING(
        rdr.seek(0);

        uint32_t bp = 0;
        uint32_t xp = 0;
        int depth = 0;
        uint32_t valstack_height = 0;

        const auto unreachable = [&]() {
            auto begin = rdr.relpos();
            for (uint32_t d = 0; (rdr.pos() != body->instructions.end) and (d > 0 or (rdr.peek<byte_t>() != ELSE and rdr.peek<byte_t>() != END)); d += ::read<instruction_t>(rdr).depth)
                ;
            auto end = rdr.relpos();

            if (end > begin) {
                std::cout << std::endl;

                std::cout << "out[" << std::setw(4) << begin << "-" << std::setw(4) << end-1 << "]: ";
                for (auto i = 0; i < depth; ++i)
                    std::cout << "  ";
                std::cout << "<unreachable code>";
            }
        };

        auto indent = [&](size_t length, bool with_block_id=false) {
            std::cout << "out[" << std::dec << std::setw(4) << rdr.relpos();
            if (with_block_id)
                std::cout << "/" << std::setw(4) << xp;
            else
                std::cout << "     ";
            std::cout << "]: ";

            for (auto i=0; i < length; ++i)
                std::cout << "  ";

            return "";
        };

        auto aux = [&](uint32_t xp, uint32_t base_ip=not_defined, uint32_t base_xp=not_defined) {
            base_ip = base_ip == not_defined ? rdr.relpos() : base_ip;
            base_xp = base_xp == not_defined ? xp : base_xp;

            std::cout << " {move " << auxs[xp].nkeep << "/" << auxs[xp].ndrop << " jmp " << base_ip + auxs[xp].ip_offset << "/" << base_xp + auxs[xp].xp_offset << "}";
            return "";
        };

        auto nvals_info = [&](int32_t nvals=-1000) {
            std::cout << " {";
            if (nvals != -1000)
                std::cout << (nvals > 0 ? "+" : "") << std::dec << nvals << " ";
            std::cout << "@" << valstack_height << "}";
            return "";
        };

        std::cout << "\nfunction(" << *signature << ")[" << nlocals << "]" << std::endl;

        while (rdr.pos() != body->instructions.end) {
            switch (rdr.peek<byte_t>()) {
                case UNREACHABLE: {
                    std::cout << indent(depth) << "unreachable";

                    ::read<byte_t>(rdr);

                    std::cout << nvals_info();

                    unreachable();
                    break;
                }
                case BLOCK: {
                    std::cout << indent(depth++) << "block";

                    ::read<byte_t>(rdr);

                    const auto bt = ::read<block_signature_type_t>(rdr);

                    ++bp;

                    std::cout << "(" << instance->resolve_block_signature_type(bt) << ")";
                    break;
                }
                case LOOP: {
                    std::cout << indent(depth++) << "loop";

                    ::read<byte_t>(rdr);

                    const auto bt = ::read<block_signature_type_t>(rdr);

                    ++bp;

                    std::cout << "(" << instance->resolve_block_signature_type(bt) << ")";
                    break;
                }
                case IF: {
                    std::cout << indent(depth++, true) << "if";

                    ::read<byte_t>(rdr);

                    const auto bt = ::read<block_signature_type_t>(rdr);

                    valstack_height -= 1;
                    ++bp;

                    std::cout << "(" << instance->resolve_block_signature_type(bt) << ")" << nvals_info(-1) << aux(xp++);
                    break;
                }
                case ELSE: {
                    std::cout << indent((--depth)++, true) << "else";

                    ::read<byte_t>(rdr);

                    valstack_height = blocks[bp].valstack_height;
                    ++bp;

                    std::cout << nvals_info() << aux(xp++);
                    break;
                }
                case END: {
                    std::cout << indent(--depth) << "end";

                    ::read<byte_t>(rdr);

                    std::cout << nvals_info();
                    break;
                }
                case BR: {
                    std::cout << indent(depth, true) << "br ";

                    ::read<byte_t>(rdr);

                    std::cout << ::read<varuint32_t>(rdr) << nvals_info() << aux(xp++);

                    unreachable();
                    break;
                }
                case BR_IF: {
                    std::cout << indent(depth, true) << "br_if ";

                    ::read<byte_t>(rdr);

                    valstack_height -= 1;

                    std::cout << ::read<varuint32_t>(rdr) << nvals_info(-1) << aux(xp++);
                    break;
                }
                case BR_TABLE: {
                    std::cout << indent(depth) << "br_table ";

                    ::read<byte_t>(rdr);

                    const auto length = ::read<varuint32_t>(rdr);

                    std::cout << length << nvals_info(-1) << std::endl;

                    //Reader trdr = rdr.clone();
                    const auto base_ip = rdr.relpos();
                    const auto base_xp = xp;
                    for (auto i = 0; i < length; ++i)
                        std::cout << indent(depth+1, true) << ::read<varuint32_t>(rdr) << aux(xp++, base_ip, base_xp) << std::endl;

                    std::cout << indent(depth+1, true) << ::read<varuint32_t>(rdr) << aux(xp++, base_ip, base_xp);

                    unreachable();
                    break;
                }
                case RETURN: {
                    std::cout << indent(depth, true) << "return";

                    ::read<byte_t>(rdr);

                    std::cout << nvals_info() << aux(xp++);

                    unreachable();
                    break;
                }
                case CALL: {
                    std::cout << indent(depth) << "call";

                    ::read<byte_t>(rdr);

                    const auto funcidx = ::read<varuint32_t>(rdr);
                    const auto fn = instance->function_space[funcidx];
                    const int32_t nvals = (int32_t) fn->signature->returns.size - (int32_t) fn->signature->params.size;

                    valstack_height += nvals;

                    std::cout << "(" << funcidx << ": " << *fn->signature << ")" << nvals_info(nvals);
                    break;
                }
                case RETURN_CALL: {
                    std::cout << indent(depth, true) << "return_call";

                    ::read<byte_t>(rdr);

                    const auto funcidx = ::read<varuint32_t>(rdr);
                    const auto fn = instance->function_space[funcidx];
                    const int32_t nvals = (int32_t) fn->signature->returns.size - valstack_height;

                    valstack_height += nvals;

                    std::cout << "(" << funcidx << ": " << *fn->signature << ")" << nvals_info(nvals) << aux(xp++);

                    unreachable();
                    break;
                }
                case CALL_INDIRECT: {
                    std::cout << indent(depth) << "call_indirect";

                    ::read<byte_t>(rdr);

                    const auto typeidx = ::read<varuint32_t>(rdr);
                    const auto tableidx = ::read<varuint32_t>(rdr);

                    const auto sig = instance->module->type_section->_.elems[typeidx];
                    check(sig->form == signature_type_t::FUNC, "Invalid call indirect signature");
                    const auto fn_sig = &sig->func;

                    const int32_t nvals = (int32_t) fn_sig->returns.size - (int32_t) fn_sig->params.size;

                    valstack_height -= 1;
                    valstack_height += nvals;

                    std::cout << "(*: " << *fn_sig << ")" << nvals_info(-1 + nvals);
                    break;
                }
                default: {
                    std::cout << indent(depth) << std::hex << (int) rdr.peek<byte_t>() << std::dec;

                    const auto instr = ::read<instruction_t>(rdr);

                    check(instr.nvals != instruction_t::var_nvals, "Expected fixed nvals");

                    valstack_height += instr.nvals;

                    std::cout << nvals_info(instr.nvals);
                    break;
                }
            }

            std::cout << std::endl;
        }

        std::cout << std::endl;
    )

    this->auxs = auxs;
}
