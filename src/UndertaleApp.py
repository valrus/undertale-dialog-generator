from flask import Flask, render_template
from PIL import Image, ImageDraw, ImageFont


app = Flask(__name__)


@app.route('/submit')
def makeDialogBox():
    return 'Hello World!'


@app.route('/')
def builder():
    return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)
