package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.ConfirmUserEmailVerificationUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class ConfirmUserEmailVerificationUseCaseImpl implements ConfirmUserEmailVerificationUseCase {

    private final UserRepository userRepository;

    @Override
    public BasicUserInfoDto execute(UUID userId) {
        userRepository.updateUserEmailVerificationStatus(userId, true);
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new BasicUserInfoDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney()
        );
    }
}
