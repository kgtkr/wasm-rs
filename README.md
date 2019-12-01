## test
```
for file in `\find . -name '*.wast'`; do
    name=`basename $file .e`
    wast2json $file -o out/$name.json
done
```