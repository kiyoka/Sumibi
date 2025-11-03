# -*- coding: utf-8 -*-
"""
sumibi.elの性能評価プログラムの一部です。
"""
import os
import re
from openai import OpenAI

def remove_reasoning_tags(text):
    """
    Remove <reasoning>...</reasoning> tags from the given text.

    Args:
        text (str): Input text that may contain reasoning tags

    Returns:
        str: Text with reasoning tags removed and stripped of extra whitespace
    """
    if text is None:
        return ""
    return re.sub(r'<reasoning>.*?</reasoning>', '', text, flags=re.DOTALL).strip()

class SumibiTypicalConvertClient:
    """
    このライブラリは、sumibi.elのsumibi-roman-to-kanji-with-surrounding()関数と同じプロンプトで、
    LLMプロバイダーのchat.completion APIを呼び出すためのものです。
    """
    def __init__(self, api_key=None, base_url=None, model=None, temperature=0.8, reasoning_effort=None, verbosity=None, timeout=None):
        """
        Initialize the SumibiTypicalConvertClient.

        Args:
            api_key (str): OpenAI API key. If None, reads from SUMIBI_AI_API_KEY or OPENAI_API_KEY env.
            base_url (str): Base URL for OpenAI API (without version). If None, reads from SUMIBI_AI_BASEURL env or uses default.
            model (str): Model name to use. If None, reads from SUMIBI_AI_MODEL env or uses "gpt-4.1".
            temperature (float): Sampling temperature for the API call.
            reasoning_effort (str): Reasoning effort level for reasoning models. If None, no reasoning_effort is used.
            verbosity (str): Verbosity level for GPT-5 models. If None, no verbosity is used.
            timeout (float): Request timeout in seconds. If None, reads from SUMIBI_AI_TIMEOUT env or uses 120 seconds.
        """
        self.api_key = api_key or os.getenv("SUMIBI_AI_API_KEY") or os.getenv("OPENAI_API_KEY")
        base_url_env = base_url or os.getenv("SUMIBI_AI_BASEURL") or "https://api.openai.com"
        # バージョン指定 (/v1) が含まれている場合は追加せず、そのまま使用
        raw_url = base_url_env.rstrip("/")
        if "/v1" in raw_url:
            self.base_url = raw_url
        else:
            self.base_url = raw_url + "/v1"
        self.model = model or os.getenv("SUMIBI_AI_MODEL") or "gpt-4.1"
        self.temperature = temperature
        self.reasoning_effort = reasoning_effort
        self.verbosity = verbosity
        # Set timeout: parameter > env var > default (120 seconds)
        if timeout is not None:
            self.timeout = timeout
        else:
            timeout_env = os.getenv("SUMIBI_AI_TIMEOUT")
            self.timeout = float(timeout_env) if timeout_env else 120.0
        self.client = OpenAI(api_key=self.api_key, base_url=self.base_url, timeout=self.timeout)

    def convert(self, surrounding_text, text):
        """
        Convert the given romaji/hiragana text to Japanese using the typical conversation flow.

        Args:
            surrounding_text (str): Contextual text around the target text.
            text (str): Romaji/hiragana text to convert.

        Returns:
            str: The converted Japanese text.
        """
        messages = [
            {
                "role": "system",
                "content": (
                    "あなたはローマ字とひらがなを日本語に変換するアシスタントです。"
                    "ローマ字の 「nn」 は 「ん」と読んでください。"
                    "[](URL)のようなmarkdown構文は維持してください。"
                    "# や ## や ### や #### のようなmarkdown構文は維持してください。"
                    "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
                    "ローマ字の字面をそのままひらがなや漢字にするだけで、元のローマ字にない文章を作り出さないでください。"
                    "出力は変換後の一文のみ。注釈や説明は一切付けないください。"
                    "もし、入力された文章が英語の文章と判断できた場合は、日本語に翻訳してください。"
                ),
            },
            {
                "role": "user",
                "content": (
                    "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
                    " 周辺の文章は、「こんにちは、中野です。watashi no namae ha nakano desu . どうぞよろしくお願いします。」"
                    "のような文章になっています。"
                    "周辺の文脈を見てそれに合った語彙を選んでください。: watashi no namae ha nakano desu ."
                ),
            },
            {"role": "assistant", "content": "私の名前は中野です。"},
            {
                "role": "user",
                "content": (
                    "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
                    "周辺の文章は、「説明はここまでです。それ以外は ikano toori desu .」"
                    "のような文章になっています。"
                    "周辺の文脈を見てそれに合った語彙を選んでください。: ikano toori desu ."
                ),
            },
            {"role": "assistant", "content": "以下の通りです。"},
            {
                "role": "user",
                "content": (
                    "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
                    f" 周辺の文章は、「{surrounding_text}」"
                    "のような文章になっています。"
                    f" 周辺の文脈を見てそれに合った語彙を選んでください。: {text}"
                ),
            },
        ]
        # Create API call parameters
        api_params = {
            "model": self.model,
            "temperature": self.temperature,
            "n": 1,
            "messages": messages
        }
        
        # Add reasoning_effort if specified
        if self.reasoning_effort is not None:
            api_params["reasoning_effort"] = self.reasoning_effort
        
        # Add verbosity if specified
        if self.verbosity is not None:
            api_params["verbosity"] = self.verbosity
        
        response = self.client.chat.completions.create(**api_params)
        content = response.choices[0].message.content
        
        # Remove <reasoning>...</reasoning> tags from the response
        return remove_reasoning_tags(content)

if __name__ == "__main__":
    # Example usage
    surrounding_text = (
        "会社ではTruly Ergonomic CLEAVE keyboardというキーボードを使っています。"
        "Truly Ergonomic CLEAVE keyboard ha nyuusyu konnnann desu ."
    )
    henkan_text = "Truly Ergonomic CLEAVE keyboard ha nyuusyu konnnann desu ."
    client = SumibiTypicalConvertClient()
    result = client.convert(surrounding_text, henkan_text)
    print(result)
