package edu.pjwstk.groups.util;

import java.security.SecureRandom;

public interface JoinCodeGenerator {
    String generate();
    String generate(int length);
}
