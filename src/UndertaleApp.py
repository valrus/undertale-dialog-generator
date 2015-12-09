from __future__ import print_function

from collections import defaultdict, namedtuple
import os

from flask import Flask, render_template, request, url_for
from PIL import Image, ImageDraw, ImageFont

BLACK, WHITE = (0, 0, 0), (255, 255, 255)

Size = namedtuple('Size', ['x', 'y'])

app = Flask(__name__)


def getFontForCharacter(character):
    if character.lower() == 'sans':
        return ImageFont.load(url_for('static', filename='css/fonts/a skele-ton.pil'))
    elif character.lower() == 'papyrus':
        return ImageFont.load(url_for('static', filename='css/fonts/NYEH HEH HEH!.pil'))
    else:
        return ImageFont.truetype(url_for('static', filename='css/fonts/DTM-Mono.otf'), 13)


def dialogBox(portrait, text, fnt):
    orig_size = Size(298, 84)
    # mode = '1' is black and white
    img = Image.new('1', orig_size)
    draw = ImageDraw.Draw(img)
    draw.fontmode = '1'
    draw.rectangle((4, 4, 294, 80), fill=1)
    draw.rectangle((7, 7, 291, 77), fill=0)
    img.paste(portrait, (13, 12))
    for row, line in enumerate(text.split('\n')[:3]):
        draw.text((77, 18 * (row + 1)), line, fill=1, font=fnt)
    return img.resize(Size(orig_size.x * 2, orig_size.y * 2))


@app.route('/submit', methods=['POST'])
def makeDialogBox():
    print(request.form)
    box = dialogBox(
        Image.open(os.path.join(app.root_path, request.form['moodImg'])),
        request.form['text'],
        getFontForCharacter(request.form['character'])
    )
    return 'test'


@app.route('/')
def builder():
    return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)
