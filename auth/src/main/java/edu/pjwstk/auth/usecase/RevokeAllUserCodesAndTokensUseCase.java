package edu.pjwstk.auth.usecase;

import java.util.UUID;

public interface RevokeAllUserCodesAndTokensUseCase {

    void execute(UUID userId);
}
