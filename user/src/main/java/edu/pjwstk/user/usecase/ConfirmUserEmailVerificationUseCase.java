package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;

import java.util.UUID;

public interface ConfirmUserEmailVerificationUseCase {
    BasicUserInfoApiDto execute(UUID userId);
}
