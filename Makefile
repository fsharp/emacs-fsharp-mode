# Directories

base_d = $(abspath .)
test_d = $(abspath test)
tmp_d  = $(abspath tmp)
bin_d  = $(abspath bin)

# Elisp files required for tests.
src_files         = $(wildcard ./*.el)
obj_files         = $(patsubst %.el,%.elc,$(src_files))
integration_tests = $(test_d)/integration-tests.el
unit_tests        = $(filter-out $(integration_tests), $(wildcard $(test_d)/*tests.el))
utils             = $(test_d)/test-common.el

# F# fontification test files
faceup_inputs  = $(wildcard $(test_d)/apps/*/*.fs) $(wildcard $(test_d)/apps/*/*.fsx)
faceup_outputs = $(patsubst %,%.faceup, $(faceup_inputs))

# Emacs command format.
emacs            = emacs
load_files       = $(patsubst %,-l %, $(utils))
load_unit_tests  = $(patsubst %,-l %, $(unit_tests))
load_integration_tests = $(patsubst %,-l %, $(integration_tests))

# Autocomplete binary distribution.
ac_name    = fsautocomplete
ac_exe     = $(bin_d)/$(ac_name).exe
ac_version = 0.30.2
ac_archive = $(ac_name)-$(ac_version).zip
ac_url     = https://github.com/fsharp/FsAutoComplete/releases/download/$(ac_version)/$(ac_name).zip

# Installation paths.
dest_root = $(HOME)/.emacs.d/fsharp-mode/
dest_bin  = $(HOME)/.emacs.d/fsharp-mode/bin/

# ----------------------------------------------------------------------------

.PHONY : test unit-test integration-test packages clean-elc install byte-compile check-compile run update-version release faceup

# Building

$(ac_archive): | $(bin_d)
	curl -L "$(ac_url)" -o "$(ac_archive)"

$(ac_exe) : $(bin_d) $(ac_archive)
	unzip "$(ac_archive)" -d "$(bin_d)"
	touch "$(ac_exe)"

~/.config/.mono/certs:
	mozroots --import --sync --quiet

install : $(ac_exe) $(dest_root) $(dest_bin)
# Install elisp packages
	$(emacs) $(load_files) --batch -f load-packages
# Copy files
	for f in $(src_files); do \
		cp $$f $(dest_root) ;\
	done
# Copy bin folder.
	cp -R $(bin_d) $(dest_root)


$(dest_root) :; mkdir -p $(dest_root)
$(dest_bin)  :; mkdir -p $(dest_bin)
$(bin_d)     :; mkdir -p $(bin_d)

# Cleaning

clean : clean-elc
	rm -rf $(bin_d)
	rm -rf $(tmp_d)
	rm -f $(ac_archive)

clean-elc :
	rm -f  *.elc
	rm -f  $(test_d)/*.elc

# Testing

test unit-test :
	HOME=$(tmp_d) ;\
	$(emacs) $(load_files) --batch -f run-fsharp-unit-tests

integration-test : $(ac_exe) packages
	HOME=$(tmp_d) ;\
	$(emacs) $(load_files) --batch -f run-fsharp-integration-tests

test-all : unit-test integration-test check-compile check-declares

packages :
	HOME=$(tmp_d) ;\
	$(emacs) $(load_files) --batch -f load-packages

byte-compile : packages
	HOME=$(tmp_d) ;\
	$(emacs) -batch --eval "(package-initialize)"\
          --eval "(add-to-list 'load-path \"$(base_d)\")" \
          -f batch-byte-compile $(src_files)

check-declares : packages
	HOME=$(tmp_d) ;\
	$(emacs) -batch --eval "(package-initialize)"\
          --eval "(dolist (file '($(foreach var,$(src_files),\"$(var)\"))) (when (check-declare-file file) (kill-emacs 1)))"

check-compile : packages $(obj_files)

faceup : $(faceup_outputs)

%.faceup : % fsharp-mode-font.el
	HOME=$(tmp_d) ;\
	$(emacs) $(load_files) -batch \
          --eval "(regen-faceup-output \"$<\")"

.el.elc:
	HOME=$(tmp_d) ;\
	$(emacs) -batch --eval "(package-initialize)"\
          --eval "(add-to-list 'load-path \"$(base_d)\")" \
          --eval '(setq byte-compile-error-on-warn t)'    \
          --eval "(eval-after-load \"bytecomp\"           \
                    '(add-to-list                         \
                        'byte-compile-not-obsolete-vars   \
                        'find-tag-marker-ring))"          \
          -f batch-byte-compile $<

run : $(ac_exe) packages
	HOME=$(tmp_d) ;\
	$(emacs) $(load_files) -f configure-fsharp-tests

# Releasing

cur_release := $(shell grep '\#\#' CHANGELOG.md | cut -d' ' -f2 | head -n 1)
prev_release := $(shell grep '\#\#' CHANGELOG.md | cut -d' ' -f2 | head -n 2 | tail -n 1)

update-version:
	sed -i -r "s/$(prev_release)/$(cur_release)/" *.el
	git add *.el CHANGELOG.md Makefile
	git commit -m "Bump version number to $(cur_release)"
	git tag -a $(cur_release) -m "Tag release $(cur_release)"

emacs-fsharp-mode-bin:
	git clone git@github.com:/rneatherway/emacs-fsharp-mode-bin

release: update-version emacs-fsharp-mode-bin $(ac_exe)
	cd emacs-fsharp-mode-bin && git pull
	cp $(bin_d)/*.exe $(bin_d)/*.exe.config $(bin_d)/*.dll emacs-fsharp-mode-bin
	cp $(src_files) emacs-fsharp-mode-bin
	cd emacs-fsharp-mode-bin && git checkout FSharp.Core.dll  # Do not change this
	cd emacs-fsharp-mode-bin && git add --all
	cd emacs-fsharp-mode-bin && git commit -m "Update to version $(cur_release)"
	cd emacs-fsharp-mode-bin && git tag -a $(cur_release) -m "Tag release $(cur_release)"


