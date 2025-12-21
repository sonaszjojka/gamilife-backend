package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserByEmailUseCase;

import java.util.Optional;

@Service
@AllArgsConstructor
public class GetUserByEmailUseCaseImpl implements GetUserByEmailUseCase {

    private final UserRepository userRepository;

    @Override
    public Optional<BasicUserInfoDto> execute(String email) {
        Optional<User> optionalUser = userRepository.getUserByEmail(email);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new BasicUserInfoDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney()
        ));
    }
}
