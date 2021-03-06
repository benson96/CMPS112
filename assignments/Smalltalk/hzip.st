#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

FileStream extend [
        |bitNumber bitCushion|

    clearByte [
         bitNumber := 8.
         bitCushion := 0.
    ]

        flushByte [
        |char|
             bitNumber = 8 ifFalse: [
                    char := Character value: bitCushion asInteger.
                    self nextPut: char.
                    self clearByte.
                  ]
    ]

    writeBit: bit [
        bitCushion isNil ifTrue: [self clearByte].
        bitNumber = 0 ifTrue: [self flushByte].
        bitCushion := bitCushion bitAt: bitNumber put: bit.
        bitNumber := bitNumber - 1.
    ]

    clearBits [
        bitNumber := 0.
    ]

    newByte [
        bitCushion := self next.
        bitNumber := 8.
    ]

    nextBit [
        |bit|
        bitNumber isNil ifTrue: [self clearBits].
        bitNumber = 0 ifTrue: [self newByte].
        bitCushion isNil ifTrue: [ ^ nil ].
        bit := bitCushion value bitAt: bitNumber.
        bitNumber := bitNumber - 1.
        ^ bit.
    ]

    bitChar: char [
        |bit charVal bitIndex|
        charVal := char value.
        bitIndex := 8.
        [bitIndex ~= 0] whileTrue: [
            bit := charVal bitAt: bitIndex.
            bitIndex := bitIndex - 1.
            self writeBit: bit.
    ].
        (charVal = 0) | (charVal = 256) ifTrue: [
            self writeBit: (charVal bitAt: 9).
        ].
    ]
    
    loadChar: freqtable [
        |ordChar|
            [self atEnd not] whileTrue: [
            ordChar := self next asInteger.
            freqtable incr: ordChar.
        ].
            ordChar = nil ifFalse: [
            freqtable incr: 256.
        ].
    ]

    loadTree [
        |stack lastBit|
        stack := Stack new: 256.
        [self atEnd not] whileTrue: [
            lastBit := self nextBit.
            lastBit = 0 ifTrue: [
                |bit bitChar bitIndex newLeaf|
                bitChar := 0.
                bitIndex := 8.
                      [bitIndex ~= 0] whileTrue: [
                      bit := self nextBit.
                      bitChar := bitChar bitAt: bitIndex put: bit.
                      bitIndex := bitIndex - 1.
                      ].
                      newLeaf := Leaf new:
                                (bitChar asCharacter) count: 0.
                      bitChar = 0 ifTrue: [
                                bit := self nextBit.
                                bit = 1 ifTrue: [
                                newLeaf := Leaf new: 256 count: 0.
                      ].
               ].
                                stack push: newLeaf.
               ].
              lastBit = 1 ifTrue: [
                |ltree rtree newTree|
                rtree := stack pop.
                stack size = 0 ifTrue: [
                    self clearBits.
                    ^ rtree
               ].
                            ltree := stack pop.
                newTree := Tree new: ltree char 
                count: 0 left: ltree right: rtree.
                stack push: newTree.
            ].
            ].
      ]

compressTo: outfile with: encodings [
        |key encode bit|
        [self atEnd not] whileTrue: [
            key := self next value.
            encode := encodings at: key.
            encode do: [:char |
                bit := char asBit.
                outfile writeBit: bit.
            ].
      ].
        encode := encodings at: 256.
        encode do: [:char |
            bit := char asBit.
            outfile writeBit: bit.
        ].
        outfile flushByte.
     ]

deCompressTo: outfile with: tree [
        |bit currTree|
             currTree := tree.
             bit := 0.
             [bit ~= nil] whileTrue: [
                        bit := self nextBit.
                        bit = 0 ifTrue: [
                                currTree := currTree left.
                        ].
                        bit = 1 ifTrue: [
                                currTree := currTree right.
                        ].
                        (currTree isMemberOf: Leaf) ifTrue: [
                          |char|
                          currTree char = 256 ifTrue: [ ^ char ].
                          char := Character value:
                                      currTree char asInteger.
                          outfile nextPut: char.
                          currTree := tree.
                        ].
                ].
       ]
]
Character extend [
        asBit [
                ^ self value bitAt: 1.
        ]

        isPrint [
                ^ ($! <= self) & (self <= $~)
        ]

        visible [
                self isPrint ifTrue: [^ self asString]
                ifFalse: [^ self asciiValue printStringRadix: 16]
        ]
]

Object subclass: Stack [
        |theArray top item|

        Stack class >> new [
                self shouldNotImplement.
        ]

        Stack class >> new: size [
                |result|
                result := super new.
                result init: size.
                ^ result
        ]

        init: size [
                top := 0.
                theArray := Array new: size.
        ]

        size [
                ^ top.
        ]

        nl := Character nl.
        pop [
                top = 0 ifTrue: [ 
                        stderr << 'stack underflow' << nl.
                        ^ nil
                ].
                item := theArray at: top.
                top := top - 1.
                ^ item
        ]

        push: item [
                top := top + 1.
                top > self size ifTrue: [
                ^ stderr << 'stack overflow' << nl.
                ].
                theArray at: top put: item.
        ]
]

Object subclass: ZeroArray [
        |theArray|

        ZeroArray class >> new [
                self shouldNotImplement.
        ]

        ZeroArray class >> new: size [
                |result|
                result := super new.
                result init: size.
                ^ result
        ]

        init: size [
                theArray := Array new: size.
        ]

        size [
                ^ theArray size.
        ]

        at: index [
                ^ theArray at: index + 1.
        ]

        at: index put: value [
                ^ theArray at: index + 1 put: value.
        ]

        incr: index [
                (self at: index) isNil
                ifTrue: [ self at: index put: 0].
                self at: index put: (self at: index) + 1.
        ]

        keysAndValuesDo: aBlock [
                (0 to: (self size) - 1) do: [:index |
                    aBlock value: index value: (self at: index).
              ]
        ]
]

"this code is based on tree.st by mackey"
Object subclass: Leaf [
        |char count|
        char [ ^ char ]
        count [ ^ count ]

        Leaf class >> new [
                self shouldNotImplement
        ]

        Leaf class >> new: aChar count: aCount [
                |result|
                result := super new.
                result setChar: aChar andCount: aCount.
                ^result
        ]

        setChar: aChar andCount: aCount [
                char := aChar.
                count := aCount.
        ]

        <= other [
                ^ (count < other count)
                | ((count = other count) & (char <= other char))
        ]

        printBase: aStream [
                ^ aStream << self class << '(' << char << ',' << count
        ]

        printOn: aStream [
               (self printBase: aStream) << ')'.
        ]

        inorder: visitor prefix: string [
               visitor value: char value: string.
        ]

        writeIn: outfile [
               outfile writeBit: (0 bitAt: 1).
               outfile bitChar: self char.
        ]
]

Leaf subclass: Tree [
        |left right|
        left [ ^ left ]
        right [ ^ right ]

        Tree class >> new: aChar count: aCount [
               self shouldNotImplement
        ]

        Tree class >> new: aChar count: aCount left:
                 aLeft right: aRight [
                |result|
                result := super new: aChar count: aCount.
                result setLeft: aLeft andRight: aRight.
                ^ result
        ]

        setLeft: aLeft andRight: aRight [
                left := aLeft.
                right := aRight.
        ]

        printOn: aStream [
                (self printBase: aStream) 
                       << ',' << left << ',' << right << ')'.
        ]

        inorder: visitor prefix: string [
                left inorder: visitor prefix: string, '0'.
                right inorder: visitor prefix: string, '1'.
        ]

        writeIn: outfile [
                left writeIn: outfile.
                right writeIn: outfile.
                outfile writeBit: (1 bitAt: 1).
        ]
]

opts := (Smalltalk getArgv: 1).
filenameIn := (Smalltalk getArgv: 2).
filenameOut := (Smalltalk getArgv: 3).

Smalltalk arguments size < 2
ifTrue: [ 
        stderr << 'Usage: ' << execname 
        << ' -dtcu inputfile [outputfile]' << nl.
        ObjectMemory quit.
].

opts =~ 't|c' ifMatched: [
freqtable := ZeroArray new: 257.
        [
                |infile|
                infile := FileStream open: filenameIn
                mode: FileStream read.
                infile loadChar: freqtable.
                infile close.
        ] on: SystemExceptions.FileError do: [:signal |
                |errno|
                errno := File errno.
                stdout flush.
                stderr << execname << ': ' << filenameIn << ': '
                          << signal messageText << nl.
                stderr flush.
        ].
].

opts =~ 't|c' ifMatched: [
        pqueue := SortedCollection new.
        freqtable keysAndValuesDo: [:key :value |
          (value notNil and: [value > 0]) ifTrue: [
                  newLeaf := Leaf new: (key asCharacter) count: value.
                  pqueue add: newLeaf.
                ]
        ].
        opts =~ 'd' ifMatched: [pqueue inspect.].
].

opts =~ 't|c' ifMatched: [
        [pqueue notEmpty] whileTrue: [
                tree := pqueue removeFirst.
                pqueue size = 0 ifTrue: [^ tree].
                rtree := pqueue removeFirst.
                newTree := Tree new: tree char 
                count: (tree count + rtree count)
                left: tree right: rtree.
                pqueue add: newTree.
        ].
        opts =~ 'd' ifMatched: [stdout << tree << nl.].
].

opts =~ 't|c' ifMatched: [
        encodings := ZeroArray new: 257.
        tree = nil ifFalse: [
                tree inorder: [:char :encoding |
                       encodings at: char asInteger put: encoding.
                ] prefix: ''.
        ].
].

opts =~ 't' ifMatched: [
        freqtable keysAndValuesDo: [:key :value |
                (value notNil and: [value > 0]) ifTrue: [
                      |character|
                      character := (key asCharacter visible).
                      character asString size to: 3 do:
                                 [:skip| stdout << ' '].
                      stdout << character << ' '. 
                      value asString size to:
                                 5 do: [:skip| stdout << ' '].
                      stdout << value << ' '.
                      stdout << (encodings at: key) << nl.
                 ]
        ]
].

opts =~ 'c' ifMatched: [
        [
                |infile outfile|
                infile := FileStream open: filenameIn
                                       mode: FileStream read.
                filenameOut = nil
                ifTrue: [
                        outfile := stdout.
                ]
                ifFalse: [
                        outfile := FileStream open: filenameOut
                                       mode: FileStream write.
                ].
                tree = nil ifFalse: [
                        tree writeIn: outfile.
                        outfile writeBit: (1 bitAt: 1).
                        outfile flushByte.
                        infile compressTo: outfile with: encodings.
                ].
                outfile close.
                infile close.
        ] on: SystemExceptions.FileError do: [:signal |
                |errno|
                errno := File errno.
                stdout flush.
                stderr << execname << ': ' << filenameOut << ': '
                       << signal messageText << nl.
                stderr flush.
        ].
].

opts =~ 'u' ifMatched: [
        [
                |infile outfile|
                infile := FileStream open: filenameIn
                                mode: FileStream read.
                tree := infile loadTree.
                tree = nil ifFalse: [
                        filenameOut = nil
                        ifTrue: [
                            infile deCompressTo: stdout with: tree.
                        ]
                        ifFalse: [
                             outfile := FileStream open: filenameOut
                                 mode: FileStream write.
                             infile deCompressTo: outfile with: tree.
                             outfile close.
                        ].
                ].
                infile close.
         ] on: SystemExceptions.FileError do: [:signal |
                |errno|
                errno := File errno.
                stdout flush.
                stderr << execname << ': ' << filenameIn << ': '
                               << signal messageText << nl.
                stderr flush.
        ].
        opts =~ 'd' ifMatched: [stdout << tree << nl.].
].
