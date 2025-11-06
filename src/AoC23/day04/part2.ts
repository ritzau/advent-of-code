#!/usr/bin/env ts-node
import * as fs from "fs";
import { solvePart2 } from "./common";

const STDIN_FD = 0;
const data = fs.readFileSync(STDIN_FD, "utf-8").trim();
solvePart2(data).then((result) => console.log(result));
