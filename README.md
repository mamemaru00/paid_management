# paid_management(有給管理システム)

## 1.OpenCOBOLのダウンロード
###windows
```
$ sudo chown $(whoami):admin /usr/local && sudo chown -R $(whoami):admin /usr/local
```

###mac
Home-brewのインストール：
```
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)
```
OpenCOBOLのインストール：
```
$ brew install open-cobol
```

参考サイト
https://www.cobol.co.jp/cobol-nyuumon/kiso/k015/

## 2.opensource COBOL+Postgresql開発環境
下記サイトの手順で構築
```
https://qiita.com/nor51010/items/9dea162cde4e34768993
```

## 3.ファイルのコンパイル実行
・従業員マスタ
```
ocesql src/DB/EMP_MASTER.cbl EMP_MASTER.pre
cobc -locesql EMP_MASTER.pre
cobcrun EMP_MASTER
```

・有給休暇残数テーブル 
```
ocesql src/DB/PL_BALANCE.cbl PL_BALANCE.pre
cobc -locesql PL_BALANCE.pre
cobcrun PL_BALANCE
```

・取得履歴
```
ocesql src/DB/PL_HISTORY.cbl PL_HISTORY.pre
cobc -locesql PL_HISTORY.pre
cobcrun PL_HISTORY
```


・有給情報従業員表示プログラム
```
ocesql src/MAIN/EMP_LIST.cbl EMP_LIST.pre
cobc -locesql EMP_LIST.pre
cobcrun EMP_LIST
```
