
template<typename T>
struct ReaderFor<Arr<T>> {
    static typename VarType<Arr<T>>::type read(Reader& rdr) {
        Arr<T> v;
        v.size = ::read<varuint32_t>(rdr);

        auto elems = new typename VarType<T>::type[v.size];
        for (auto i = 0; i < v.size; ++i)
            elems[i] = ::read<T>(rdr);
        v.elems = elems;

        return v;
    }
};
