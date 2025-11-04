package edu.pjwstk.auth.usecase;

import java.util.UUID;

public interface ResendEmailVerificationCodeUseCase {
    void execute(UUID userId);
}
