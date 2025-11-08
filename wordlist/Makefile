# SCOWL (Spell Checker Oriented Word Lists) から英単語辞書を生成する
# データソース: http://wordlist.aspell.net/
# ライセンス: Public Domain

SCOWL_VERSION = 2020.12.07
SCOWL_URL = http://downloads.sourceforge.net/wordlist/scowl-$(SCOWL_VERSION).tar.gz
SCOWL_TARFILE = scowl-$(SCOWL_VERSION).tar.gz
SCOWL_DIR = scowl-$(SCOWL_VERSION)
WORDLIST_FILE = $(SCOWL_DIR)/final/english-words.10

.PHONY: all download generate clean

# デフォルトターゲット: ダウンロードと生成を両方実行
all: download generate

# SCOWLをダウンロードして展開
download: $(WORDLIST_FILE)

$(WORDLIST_FILE): $(SCOWL_TARFILE)
	tar xzf $(SCOWL_TARFILE)
	touch $(WORDLIST_FILE)

$(SCOWL_TARFILE):
	wget $(SCOWL_URL)

# 英単語辞書ファイルを生成
generate: ../lisp/sumibi-english-words.el

../lisp/sumibi-english-words.el: $(WORDLIST_FILE) gen_english_words.py
	python3 gen_english_words.py $(WORDLIST_FILE) > ../lisp/sumibi-english-words.el

# クリーンアップ
clean:
	rm -rf $(SCOWL_DIR) $(SCOWL_TARFILE) ../lisp/sumibi-english-words.el
