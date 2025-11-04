package edu.pjwstk.auth.usecase.resendemailverification;

import java.util.UUID;

public interface ResendEmailVerificationCodeUseCase {
    void execute(UUID userId);
}
