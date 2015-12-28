from __future__ import print_function, unicode_literals

from base64 import b64encode
from collections import namedtuple
from cStringIO import StringIO
from tempfile import mkstemp
import os
import time

from flask import Flask, render_template, request, after_this_request, make_response
from PIL import Image, ImageDraw, ImageFont

from personalities import apply_personality

BLACK, WHITE = (0, 0, 0), (255, 255, 255)

Size = namedtuple('Size', ['x', 'y'])

UPLOAD_FOLDER = 'static/images/boxes'

app = Flask(__name__)

app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

BASIC_UNICODE_SANITIZER = {
    0x2018: '\'',  # LEFT SINGLE QUOTATION MARK
    0x2019: '\'',  # RIGHT SINGLE QUOTATION MARK
    0x201c: '"',   # LEFT DOUBLE QUOTATION MARK
    0x201d: '"',   # RIGHT DOUBLE QUOTATION MARK
    0x00a0: ' ',   # NO-BREAK SPACE
}


def _clean_text(text, character):
    sanitized = text.translate(BASIC_UNICODE_SANITIZER)
    if character.lower() == 'sans':
        return sanitized.lower()
    elif character.lower() == 'papyrus':
        return sanitized.upper()
    else:
        return sanitized


def _indent(line_num, line):
    if line_num:
        return '  ' + line
    else:
        return '* ' + line


def getFontForCharacter(character):
    font_dir = os.path.join(app.root_path, 'static', 'css', 'fonts')
    if character.lower() == 'sans':
        return ImageFont.load(os.path.join(font_dir, 'a skele-ton.pil'))
    elif character.lower() == 'papyrus':
        return ImageFont.load(os.path.join(font_dir, 'NYEH HEH HEH!.pil'))
    else:
        return ImageFont.truetype(os.path.join(font_dir, 'DTM-Mono.otf'), 13)


def dialogBox(portrait, text, fnt, doIndent=True):
    orig_size = Size(298, 84)
    # mode = '1' is black and white
    img = Image.new(b'1', orig_size)
    draw = ImageDraw.Draw(img)
    draw.fontmode = b'1'
    draw.rectangle((4, 4, 294, 80), fill=1)
    draw.rectangle((7, 7, 291, 77), fill=0)
    img.paste(portrait, (13, 12))
    for row, line in enumerate(text.split('\n')[:3]):
        print('"{}"'.format(repr(line)), draw.textsize(line, font=fnt))
        draw.text((77, 16 + row * 18),
                  _indent(row, line) if doIndent else line,
                  fill=1, font=fnt)
    return img.resize(Size(orig_size.x * 2, orig_size.y * 2))


@app.route('/submit', methods=['GET'])
def makeDialogBox():
    character = request.args.get('character').lower()
    text = _clean_text(request.args.get('text'), character)
    imgData = apply_personality(text, character)
    if imgData is None:
        mood_img = request.args.get('moodImg')
        box = dialogBox(
            Image.open(mood_img.lstrip('/')),
            text,
            getFontForCharacter(character),
            doIndent=(character != 'papyrus')
        )
        stream = StringIO()
        box.save(stream, format='png', optimize=True)
        imgData = b64encode(stream.getvalue())

        @after_this_request
        def cleanup(response):
            stream.close()
            return response

    print(imgData)
    return imgData


@app.route('/')
def builder():
    response = make_response(render_template('index.html'))
    response.headers['X-Clacks-Overhead'] = 'GNU Terry Pratchett'
    return response


if __name__ == '__main__':
    app.run(debug=True)
