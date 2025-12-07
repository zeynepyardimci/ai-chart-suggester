"""
Test script to list available Gemini models
"""
import google.generativeai as genai

# Configure API
GEMINI_API_KEY = "AIzaSyCzI59wKr717ucBpYPVUnUbhKNH5K0QG2o"
genai.configure(api_key=GEMINI_API_KEY)

print("Available Gemini Models:")
print("=" * 50)

for model in genai.list_models():
    if 'generateContent' in model.supported_generation_methods:
        print(f"\nModel: {model.name}")
        print(f"  Display Name: {model.display_name}")
        print(f"  Supported Methods: {model.supported_generation_methods}")
