package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.CheckIfUsersEmailIsVerifiedApiDto;

import java.util.UUID;

public interface CheckIfUsersEmailIsVerifiedUseCase {
    CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId);
}
