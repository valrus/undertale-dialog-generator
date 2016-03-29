from __future__ import print_function, unicode_literals

from base64 import b64encode
from collections import namedtuple
from cStringIO import StringIO
import os

from flask import Flask, render_template, request, after_this_request
from flask import make_response, jsonify
from flask.ext.compress import Compress
from PIL import Image, ImageDraw, ImageFont

from personalities import apply_personality, PersonalityOverrideException


from imgurpython import ImgurClient
IMGUR_CLIENT_ID = os.environ.get('IDTHV_IMGUR_ID')
IMGUR_CLIENT_SECRET = os.environ.get('IDTHV_IMGUR_SECRET')
IMGUR_ALBUM_ID = os.environ.get('IDTHV_ALBUM_ID')
imgur_client = ImgurClient(IMGUR_CLIENT_ID, IMGUR_CLIENT_SECRET)


# Cache HTML template for a day
HTML_CACHE_AGE = 60 * 60 * 24


BLACK, WHITE = (0, 0, 0), (255, 255, 255)
Size = namedtuple('Size', ['x', 'y'])


UPLOAD_FOLDER = 'static/images/boxes'
app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

Compress(app)
# 'application/x-font-woff' omitted because compression ratio is 1.0x
app.config['COMPRESS_MIMETYPES'] += ['text/plain']


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


def get_font_for_character(character):
    font_dir = os.path.join(app.root_path, 'static', 'css', 'fonts')
    if character.lower() == 'sans':
        return ImageFont.load(os.path.join(font_dir, 'a skele-ton.pil'))
    elif character.lower() == 'papyrus':
        return ImageFont.load(os.path.join(font_dir, 'NYEH HEH HEH!.pil'))
    else:
        return ImageFont.truetype(os.path.join(font_dir, 'DTM-Mono.otf'), 13)


def chunks(lst, length, max_chunks=3):
    left = max_chunks
    for i in xrange(0, len(lst), length):
        if left == 0:
            yield lst[i:]
            break
        yield lst[i:i + length]
        left -= 1


def get_character_from_portrait(src):
    return os.path.basename(os.path.dirname(src)).lower()


def dialogBox(portraits, text):
    textChunks = list(chunks(text.split('\n'), 3))
    orig_size = Size(298, 4 + 84 * len(textChunks))
    # mode = '1' is black and white
    img = Image.new(b'1', orig_size)
    draw = ImageDraw.Draw(img)
    draw.fontmode = b'1'
    for boxNum, (portrait_src, text_chunk) in enumerate(zip(portraits, textChunks)):
        character = get_character_from_portrait(portrait_src)
        apply_personality(' '.join(text_chunk), character)
        box_text = [_clean_text(line, character) for line in text_chunk]
        fnt = get_font_for_character(character)
        y_offset = 84 * boxNum
        draw.rectangle((4, 4 + y_offset, 294, 80 + y_offset), fill=1)
        draw.rectangle((7, 7 + y_offset, 291, 77 + y_offset), fill=0)
        img.paste(Image.open(portrait_src.lstrip('/')), (13, 12 + y_offset))
        for row, line in enumerate(box_text[:3]):
            if app.debug:
                print('"{}"'.format(repr(line)), draw.textsize(line, font=fnt))
            draw.text((77, y_offset + 16 + row * 18),
                      _indent(row, line) if character != 'papyrus' else line,
                      fill=1, font=fnt)
    return img.resize(Size(orig_size.x * 2, orig_size.y * 2))


@app.route('/submit', methods=['GET'])
def makeDialogBox():
    text = request.args.get('text')
    try:
        mood_imgs = request.args.getlist('moodImg')
        if app.debug:
            print(mood_imgs)
        box = dialogBox(mood_imgs, text)
        stream = StringIO()
        box.save(stream, format='png', optimize=True)
        img_data = b64encode(stream.getvalue())

        @after_this_request
        def cleanup(response):
            stream.close()
            return response
    except PersonalityOverrideException as override_response:
        img_data = override_response.img_data

    if app.debug:
        print(img_data)
    return img_data


@app.route('/imgur_id', methods=['GET'])
def getImgurId():
    return jsonify(clientId=IMGUR_CLIENT_ID, albumId=IMGUR_ALBUM_ID)


@app.route('/')
def builder():
    response = make_response(render_template('index.html'))
    response.headers['X-Clacks-Overhead'] = 'GNU Terry Pratchett'
    response.cache_control.max_age = HTML_CACHE_AGE
    response.add_etag()
    return response


if __name__ == '__main__':
    app.run(debug=True)
