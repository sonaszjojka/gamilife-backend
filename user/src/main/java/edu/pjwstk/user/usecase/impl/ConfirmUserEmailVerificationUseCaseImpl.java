package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.ConfirmUserEmailVerificationUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class ConfirmUserEmailVerificationUseCaseImpl implements ConfirmUserEmailVerificationUseCase {

    private final UserRepository userRepository;

    @Override
    public BasicUserInfoApiDto execute(UUID userId) {
        userRepository.updateUserEmailVerificationStatus(userId, true);
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new BasicUserInfoApiDto(
                user.getId(),
                user.getEmail(),
                user.getUsername()
        );
    }
}
