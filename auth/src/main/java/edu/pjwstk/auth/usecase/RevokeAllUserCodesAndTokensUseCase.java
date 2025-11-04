package edu.pjwstk.auth.usecase;

import java.util.UUID;

@Deprecated
public interface RevokeAllUserCodesAndTokensUseCase {

    void execute(UUID userId);
}
