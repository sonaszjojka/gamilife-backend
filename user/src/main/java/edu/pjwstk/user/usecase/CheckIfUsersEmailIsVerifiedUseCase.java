package edu.pjwstk.user.usecase;

import edu.pjwstk.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;

import java.util.UUID;

public interface CheckIfUsersEmailIsVerifiedUseCase {
    CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId);
}
