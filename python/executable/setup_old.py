from setuptools import setup

APP = ['OM-Pursuit.py']
DATA_FILES = []
OPTIONS = {'argv_emulation': True, 'excludes' : ['PyQt4', 'PyQt4.QtCore', 'PyQt4.GUI', 'matplotlib', 'sip', 'email', 'pydoc'], 'includes' : ['scipy.signal', 'scipy.linalg', 'scipy.interpolate']}

setup(
    app=APP,
    data_files=DATA_FILES,
    options={'py2app': OPTIONS},
    setup_requires=['py2app'],
)
