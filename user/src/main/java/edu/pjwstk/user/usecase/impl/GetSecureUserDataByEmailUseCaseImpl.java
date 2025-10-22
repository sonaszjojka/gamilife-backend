package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetSecureUserDataByEmailUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
                user.getPassword(),
                user.getPasswordChangeDate(),
                user.isEmailVerified()
        ));
    }
}
