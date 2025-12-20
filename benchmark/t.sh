#!/bin/bash -x
#export SUMIBI_AI_API_KEY="your-gemini-api-key"
export SUMIBI_AI_BASEURL="https://generativelanguage.googleapis.com/v1beta"
export SUMIBI_AI_MODEL=gemini-3-flash-preview
make result_ver2.4.0/${SUMIBI_AI_MODEL}.json
make result_ver2.4.0/${SUMIBI_AI_MODEL}_katakana.json
make result_ver2.4.0/${SUMIBI_AI_MODEL}_hiragana.json
