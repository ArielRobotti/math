import Prim "mo:⛔";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
module {

    public module linealAlg {

        public func shape<T>(m : [[Int]]) : (Nat, Nat) { (rows<T>(m), cols<T>(m))};

        func rows<T>(m : [[Int]]) : Nat { return m.size() };

        func cols<T>(m : [[Int]]) : Nat { return m[0].size() };

        public func map(m : [[Int]], f : Int -> Int) : [[Int]] {
            let rowsBuffer = Buffer.fromArray<[Int]>([]);
            for (r in m.vals()) {
                rowsBuffer.add(Prim.Array_tabulate<Int>(cols(m), func i = f(r[i])));
            };
            Buffer.toArray<[Int]>(rowsBuffer);
        };

        public func dotProduct(m1 : [[Int]], m2 : [[Int]]) : Int {
            let shapeM = shape(m1);
            if (shape(m2) != shapeM) {
                Prim.trap("Incompatible shapes");
            };
            var result : Int = 0;
            for (i in Iter.range(0, shapeM.0 -1)) {
                for (j in Iter.range(0, shapeM.1 -1)) {
                    result += m1[i][j] * m2[i][j];
                };
            };
            result;
        };

        public func fillWith(m : [[Int]], val : Int) : [[Int]] {
            let rows = m.size();
            var cols = m[0].size();
            for (i in Iter.range(0, m.size() - 1)) {
                if (m[i].size() > cols) { cols := m[i].size() };
            };
            func fillM(r : Nat, c : Nat) : Int {
                if (c < m[r].size()) {
                    m[r][c];
                } else {
                    val;
                };
            };
            tabulate<[Int]>(rows, func r = tabulate<Int>(cols, func c = fillM(r, c)));
        };

        let tabulate = Prim.Array_tabulate;

        public func transpose(m : [[Int]]) : [[Int]] {
            let mr = fillWith(m, 0);
            let (rows, cols) = shape(mr);
            tabulate<[Int]>(cols, func r = tabulate<Int>(rows, func c = mr[c][r]));
        };

        public func removeRow(m : [[Int]], _r : Nat) : [[Int]] {
            if (_r >= m.size()) { return m };
            let tempBuffer = Buffer.fromArray<[Int]>(m);
            ignore tempBuffer.remove(_r);
            Buffer.toArray(tempBuffer);
        };
        public func removeCol(m : [[Int]], _c : Nat) : [[Int]] {
            let (rows, cols) = shape(m);
            if (_c >= cols) { return m };
            tabulate<[Int]>(rows, func r = tabulate<Int>(cols - 1, func c = if (c < _c) { m[r][c] } else { m[r][c +1] }));
        };

        
        public func det(m : [[Int]]) : Int {
            //Recursive calculation of the determinant of nxn matrices by the method of cofactors, 
            //and by the Sarrus method in the base case of the recursion
            let shapeM = shape(m);
            let order = shapeM.0;
            if (shapeM.0 != shapeM.1) {
                Prim.trap("The matrix entered must be square");
            };
            if (shape(m) == (3, 3)) {
                // Case base: Sarrus rule
                return (m[0][0] * m[1][1] * m[2][2]) +
                (m[0][1] * m[1][2] * m[2][0]) +
                (m[0][2] * m[1][0] * m[2][1]) -
                (m[0][2] * m[1][1] * m[2][0]) -
                (m[1][2] * m[2][1] * m[0][0]) -
                (m[2][2] * m[0][1] * m[1][0]);
            } else {
                func selecRowOrCol() : (?Nat, ?Nat) {

                    let rows = Prim.Array_init<Nat>(order, 0);
                    let cols = Prim.Array_init<Nat>(order, 0);
                    var max = 0;
                    var result : (?Nat, ?Nat) = (?0, null);

                    for (r in Iter.range(0, order - 1)) {
                        for (c in Iter.range(0, order - 1)) {
                            if (m[r][c] == 0) {
                                rows[r] += 1;
                                cols[c] += 1;
                            };
                        };
                    };
                    for (i in Iter.range(0, order - 1)) {
                        if (cols[i] > max) {
                            max := cols[i];
                            result := (null, ?i);
                        };
                        if (rows[i] > max) {
                            max := rows[i];
                            result := (?i, null);
                        };
                    };
                    result;
                };

                var determ = 0 : Int;
                return switch (selecRowOrCol()) {
                    case (null, ?col) {
                        for (i in Iter.range(0, order - 1)) {
                            let subM = removeCol(removeRow(m, i), col);
                            determ += ((-1) ** (i + col)) * m[i][col] * det(subM);
                        };
                        determ;
                    };
                    case (?row, null) {
                        for (i in Iter.range(0, order - 1)) {
                            let subM = removeCol(removeRow(m, row), i);
                            if (m[row][i] != 0){
                               determ +=  ((-1) ** (i + row)) * m[row][i] * det(subM); 
                            };
                        };
                        determ;
                    };
                    case _ { Prim.trap("Unspected error") };

                };
            };
        };
    };

};
