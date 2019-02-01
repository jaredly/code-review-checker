set -ex
echo Hi
rm -rf Phabrador.app
./macapp.sh Phabrador icon.png
DEST=Phabrador.app/Contents/MacOS
esy cp "#{self.target_dir / 'install' / 'default' / 'bin' / 'PhabradorProd'}" $DEST/Phabrador
cp -r ../assets   Phabrador.app/Contents/MacOS/
git rev-parse HEAD > Phabrador.app/Contents/MacOS/assets/git-head
zip -r Phabrador.zip Phabrador.app