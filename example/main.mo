import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";

import { linealAlg } "../src/math";

actor {

    public type Matrix = [var [var Int]];

    public query func maps(m : [[Int]]) : async [[Int]] {
        linealAlg.map(m, func x = x ** 2);
    };

    public query func transpose(m : [[Int]]) : async [[Int]] {
        linealAlg.transpose(m);
    };

    public query func fill(m : [[Int]], val : Int) : async [[Int]] {
        linealAlg.fillWith(m, val);
    };

    public query func removeRow(m : [[Int]], r : Nat) : async [[Int]] {
        linealAlg.removeRow(m, r);
    };
    public query func removeCol(m : [[Int]], c : Nat) : async [[Int]] {
        linealAlg.removeCol(m, c);
    };

    public query func dotProduct(m1 : [[Int]], m2 : [[Int]]) : async Int {
        linealAlg.dotProduct(m1, m2);
    };

    public query func det(m : [[Int]]) : async Int {
        linealAlg.det(m);
    };

};
