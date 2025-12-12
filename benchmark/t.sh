#!/bin/bash -x
#export SUMIBI_AI_API_KEY="sk-proj-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
# For OpenAI API, SUMIBI_AI_BASEURL can be omitted or set to https://api.openai.com/v1
# export SUMIBI_AI_BASEURL=https://api.openai.com/v1
export SUMIBI_AI_MODEL=gpt-5.2
make result_ver2.4.0/${SUMIBI_AI_MODEL}.json
make result_ver2.4.0/${SUMIBI_AI_MODEL}_katakana.json
make result_ver2.4.0/${SUMIBI_AI_MODEL}_hiragana.json
