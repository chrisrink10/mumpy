"""setup.py

Licensed under a BSD license. See LICENSE for more information.

Author: Christopher Rink"""
try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name='mumpy',
    version='0.1',
    packages=['mumpy'],
    url='github.com/chrisrink10/mumpy',
    license='3-Clause BSD License',
    author='Christopher Rink',
    author_email='chrisrink10@gmail.com',
    description='ANSI M interpreter'
)
