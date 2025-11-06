#!/usr/bin/env ts-node
import * as fs from "fs";
import { solvePart1 } from "./common";

const STDIN_FD = 0;
const data = fs.readFileSync(STDIN_FD, "utf-8").trim();
solvePart1(data).then((result) => console.log(result));
