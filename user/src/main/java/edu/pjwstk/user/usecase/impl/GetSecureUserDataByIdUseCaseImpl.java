package edu.pjwstk.user.usecase.impl;

import pl.gamification.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetSecureUserDataByIdUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GetSecureUserDataByIdUseCaseImpl implements GetSecureUserDataByIdUseCase {

    private UserRepository userRepository;

    @Override
    public Optional<SecureUserInfoApiDto> execute(UUID userId) {
        Optional<User> optionalUser = userRepository.getUserById(userId);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new SecureUserInfoApiDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getPassword(),
                user.getPasswordChangeDate(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        ));
    }
}
