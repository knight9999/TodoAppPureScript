# プロジェクトの新規作成

```
% mkdir TodoApp
% cd TodoApp
% npm init -y
% npm install spago purescript
% npx spago init
% npx spago install halogen
```

# ディレクトリ作成

```
% mkdir public
```

# git ignoreに追加

```
/public/app.js
```

```
% git init
```

# pacakge.json

```
    "test": "spago test",
    "build": "spago bundle-app --to public/app.js"
```

# 参考サイト

基礎から学ぶVue.js 
ToDoリストを作りながら学習しよう！
https://cr-vue.mio3io.com/tutorials/todo.html#%E5%AE%8C%E6%88%90%E5%BD%A2



