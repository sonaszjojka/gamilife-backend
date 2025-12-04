package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.ConfirmUserEmailVerificationUseCase;

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
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney()
        );
    }
}
