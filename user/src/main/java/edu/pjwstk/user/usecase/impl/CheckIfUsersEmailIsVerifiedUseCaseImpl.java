package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.common.userApi.dto.CheckIfUsersEmailIsVerifiedApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.CheckIfUsersEmailIsVerifiedUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CheckIfUsersEmailIsVerifiedUseCaseImpl implements CheckIfUsersEmailIsVerifiedUseCase {

    private UserRepository userRepository;

    @Override
    public CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new CheckIfUsersEmailIsVerifiedApiDto(
                user.isEmailVerified(),
                user.getEmail()
        );
    }
}
