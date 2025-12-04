package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.SecureUserInfoApiDto;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetSecureUserDataByEmailUseCase;

import java.util.Optional;

@Service
@AllArgsConstructor
public class GetSecureUserDataByEmailUseCaseImpl implements GetSecureUserDataByEmailUseCase {

    private UserRepository userRepository;

    @Override
    public Optional<SecureUserInfoApiDto> execute(String email) {
        Optional<User> optionalUser = userRepository.getUserByEmail(email);

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
