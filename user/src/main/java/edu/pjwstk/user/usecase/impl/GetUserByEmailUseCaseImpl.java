package edu.pjwstk.user.usecase.impl;

import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetUserByEmailUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
public class GetUserByEmailUseCaseImpl implements GetUserByEmailUseCase {

    private final UserRepository userRepository;

    @Override
    public Optional<BasicUserInfoApiDto> execute(String email) {
        Optional<User> optionalUser = userRepository.getUserByEmail(email);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new BasicUserInfoApiDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney()
        ));
    }
}
