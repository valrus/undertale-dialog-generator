from __future__ import print_function

from base64 import b64encode
from collections import namedtuple
from cStringIO import StringIO
from tempfile import mkstemp
import os
import time

from flask import Flask, render_template, request, url_for, send_from_directory, after_this_request, jsonify
from PIL import Image, ImageDraw, ImageFont

BLACK, WHITE = (0, 0, 0), (255, 255, 255)

Size = namedtuple('Size', ['x', 'y'])

UPLOAD_FOLDER = 'static/images/boxes'

app = Flask(__name__)

app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER


def getFontForCharacter(character):
    font_dir = os.path.join(app.root_path, 'static', 'css', 'fonts')
    if character.lower() == 'sans':
        return ImageFont.load(os.path.join(font_dir, 'a skele-ton.pil'))
    elif character.lower() == 'papyrus':
        return ImageFont.load(os.path.join(font_dir, 'NYEH HEH HEH!.pil'))
    else:
        return ImageFont.truetype(os.path.join(font_dir, 'DTM-Mono.otf'), 13)


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
        draw.text((77, 16 + row * 18), line, fill=1, font=fnt)
    return img.resize(Size(orig_size.x * 2, orig_size.y * 2))


@app.route('/submit', methods=['GET'])
def makeDialogBox():
    print(request.form)
    character = request.args.get('character')
    box = dialogBox(
        Image.open(request.args.get('moodImg').lstrip('/')),
        request.args.get('text'),
        getFontForCharacter(character)
    )
    # fd, box_file = mkstemp(prefix='undertale_{}_'.format(character),
    #                        suffix='.png',
    #                        dir=app.config['UPLOAD_FOLDER'])
    # os.close(fd)
    stream = StringIO()
    box.save(stream, format='png')

    @after_this_request
    def cleanup(response):
        stream.close()
        return response

    return b64encode(stream.getvalue())
    # return os.path.join(app.config['UPLOAD_FOLDER'], os.path.basename(box_file))
    # result = send_from_directory(app.config['UPLOAD_FOLDER'],
    #                              box_file, as_attachment=True)
    # print(result.__dict__)
    # return url_for('static',
    #                filename='{}/{}'.format(app.config['UPLOAD_FOLDER'],
    #                                        box_file))


@app.route('/')
def builder():
    return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)
