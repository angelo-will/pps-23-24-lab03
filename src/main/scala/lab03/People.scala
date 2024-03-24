package lab03

import u03.Sequences.*
import Sequence.*

object People extends App:
    
    enum Person:
        case Student(name: String, year: Int )
        case Teacher(name: String, course: String)

