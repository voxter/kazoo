#!/usr/bin/env python3

from jinja2 import Template
import json
import sys
import os
import re
import datetime
import subprocess
import yaml
import requests
from requests.auth import HTTPBasicAuth

class CircleEnv(object):

    # getters for all the environment and shipyard stuff

    @property
    def shipyard(self):
        if not hasattr(self, '_shipyard'):
            self._shipyard = self.get_shipyard()
        return self._shipyard

    @property
    def package_data(self):
        return self.shipyard['package']['centos7']

    @property
    def base_branch(self):
        if 'base_branch' in self.shipyard.keys():
            return self.shipyard['base_branch']
        return None

    @property
    def base_core(self):
        if 'base_core' in self.shipyard.keys():
            return self.shipyard['base_core']
        return None

    @property
    def name(self):
        return self.shipyard['name']

    @property
    def base_url(self):
        if not hasattr(self, '._base_url'):
            self._base_url = self.make_base_url()
        return self._base_url

    @property
    def build_root(self):
        if not hasattr(self, '_build_root'):
            self._build_root = self.get_env('BUILD_ROOT')
        return self._build_root

    @property
    def source_dir(self):
        if not hasattr(self, '_source_dir'):
            self._source_dir = os.path.join(self.build_root, 'SOURCES')
        return self._source_dir

    @property
    def source_tar(self):
        return self.get_env('SOURCE_TAR')

    @property
    def spec_dir(self):
        if not hasattr(self, '_spec_dir'):
            self._spec_dir = os.path.join(self.build_root, 'SPECS')
        return self._spec_dir

    @property
    def ci_root(self):
        if not hasattr(self, '_ci_root'):
            self._ci_root = os.path.dirname(self.build_root)
        return self._ci_root

    @property
    def app_dir(self):
        if not hasattr(self, '_app_dir'):
            self._app_dir = self.get_env('APP_DIR')
        return self._app_dir

    @property
    def ci_metadata_file(self):
        if not hasattr(self, '_metadata_file'):
            if self.app_dir:
                self._metadata_file = os.path.join(self.app_dir, 'metadata/app.json')
            else:
                self._metadata_file = None
        return self._metadata_file

    @property
    def artifact_dir(self):
        if not hasattr(self, '_artifact_dir'):
            self._artifact_dir = os.path.join(self.build_root, 'RPMS')
        return self._artifact_dir

    @property
    def pull_request(self):
        if not hasattr(self, '_pull_request'):
            if self.get_env('CIRCLE_PULL_REQUEST'):
                self._pull_request = os.path.basename(self.get_env('CIRCLE_PULL_REQUEST'))
            else:
                self._pull_request = None
        return self._pull_request

    @property
    def tag(self):
        if not hasattr(self, '_tag'):
            self._tag = self.get_env('CIRCLE_TAG')
        return self._tag

    @property
    def branch(self):
        if not hasattr(self, '_branch'):
            self._branch = self.get_env('CIRCLE_BRANCH')
        return self._branch

    @property
    def version(self):
        if not hasattr(self, '_version'):
            self._version = self.get_env('VERSION')
        return self._version

    @property
    def release(self):
        if not hasattr(self, '_release'):
            self._release = self.get_env('RELEASE')
        return self._release

    @property
    def package_name(self):
        if not hasattr(self, '_package_name'):
            self._package_name = self.get_env('PACKAGE_NAME')
        return self._package_name

    @property
    def template(self):
        self._template = self.shipyard['template']
        return self._template

    @property
    def template_file(self):
        if not hasattr(self, '_template_file'):
            self._template_file = os.path.join(self.build_root, self.template)
        return self._template_file

    @property
    def spec_file(self):
        if not hasattr(self, '_spec_file'):
            if self.template.endswith('.tmpl'):
                spec_name = self.template[:-5]
                self._spec_file = os.path.join(self.spec_dir, spec_name)
        return self._spec_file

    @property
    def github_user(self):
        if not hasattr(self, '_github_user'):
            self._github_user = self.get_env('CIRCLE_GH_USER')
        return self._github_user

    @property
    def github_token(self):
        if not hasattr(self, '_github_token'):
            self._github_token = self.get_env('CIRCLE_GH_TOKEN')
        return self._github_token

    @property
    def project_username(self):
        if not hasattr(self, '_project_username'):
            self._project_username = self.get_env('CIRCLE_PROJECT_USERNAME')
        return self._project_username

    @property
    def project_reponame(self):
        if not hasattr(self, '_project_reponame'):
            self._project_reponame = self.get_env('CIRCLE_PROJECT_REPONAME')
        return self._project_reponame

    @property
    def build_number(self):
        if not hasattr(self, '_build_number'):
            self._build_number = self.get_env('CIRCLE_BUILD_NUM')
        return self._build_number

    @property
    def metadata_file(self):
        if not hasattr(self, '_metadata_file'):
            self._metadata_file = os.path.join(self.app_dir, 'metadata/app.json')
        return self._metadata_file

    # condition functions

    def is_master(self):
        return self.branch == 'master'

    def is_release_branch(self):
        return re.match('^[0-9]+\.[0-9]+', self.branch)


    def is_pull_request(self):
        if self.pull_request:
            return True
        return False

    def is_release(self):
        if self.tag:
            return True
        return False

    # Util functions

    def get_env(self, name, default=None):
        return os.environ[name] if name in os.environ.keys() else None

    def get_shipyard(self):
        if not hasattr(self, 'shipyard_data'):
            self.shipyard_data = self.maybe_json_shipyard()
            if not self.shipyard_data:
                self.shipyard_data = self.maybe_yaml_shipyard()
        if not self.shipyard_data:
            sys.exit("you done goofed, you forgot to create a shipyard file for this project!")
        return self.shipyard_data

    def maybe_yaml_shipyard(self):
        self.shipyard_file = os.path.join(self.app_dir, '.shipyard.yml')
        if not os.path.exists(self.shipyard_file):
            return None
        with open(self.shipyard_file) as yaml_file:
            return yaml.load(yaml_file, Loader=yaml.SafeLoader)

    #TODO: deprecate this when all the repos are updated to use YAML.
    def maybe_json_shipyard(self):
        self.shipyard_file = os.path.join(self.app_dir, '.shipyard.json')
        if not os.path.exists(self.shipyard_file):
            return None
        with open(self.shipyard_file) as json_file:
            return json.load(json_file)

    def run_cmd(self, cmd, fail=True):
        result = subprocess.run(cmd, stdout=subprocess.PIPE)
        if fail:
            result.check_returncode()
        return result.stdout.decode('ascii').strip()

    def get_pr_parent_branch(self):
        pr = self.fetch_gh(self.base_url + "pulls/{}".format(self.pull_request))
        return pr['base']['ref']

    def fetch_gh(self, url):
        response = requests.get(url)
        if response.status_code == 404:
            if self.github_token and self.github_user:
                response = requests.get(url, auth=(self.github_user, self.github_token))
            else:
                sys.exit("invalid request, this repo is private, maybe you forgot to include something?")
        if response.status_code == 200:
            return response.json()
        response.raise_for_status()

    def make_base_url(self):
        return "https://api.github.com/repos/{}/{}/".format(self.project_username, self.project_reponame)

    def tag_to_pkg_version(self, tag):
        return self.tag_to_version(tag) + '-' + self.tag_to_release(tag)

    def tag_to_version(self, tag):
        parts = tag.split('.')
        release = parts.pop()
        version = '.'.join(parts)
        return version

    def tag_to_release(self, tag):
        parts = tag.split('.')
        return self.parse_release(parts.pop())

    def parse_release(self, release_string):
        #TODO, update dist with minor bump
        if release_string.find(':') != -1:
            epoch, release_string = release_string.split(':')
        if release_string.find('-') != -1:
            release_string, minorbump = release_string.split('-')
        #this is for when kamailio needs those whacky extraver and snapinfo fields those MUST be included in the tag (eg: major.minor.release_extraver_minorbump)
        if release_string.find('_') != -1:
            parts = release_string.split('_')
            release_string = '.'.join(parts)
        #if minorbump:
        #    return release_string + '.' + minorbump
        return release_string

if __name__ == "__main__":
    CircleEnv()
