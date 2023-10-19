# SKK-JISYO.L.unannotatedから同音意義語だけを抜き出した辞書を作成する。
# 辞書は、Emacs Lispファイルとして保存する。
../lisp/sumibi-localdic.el : SKK-JISYO.L.unannotated gen_localsumibidic.py
	python3 gen_localsumibidic.py SKK-JISYO.L.unannotated > ../lisp/sumibi-localdic.el
