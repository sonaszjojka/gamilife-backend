package pl.gamilife.user.usecase.impl;

import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.response.GetUsersResult;
import pl.gamilife.user.dto.response.UserDetailsResponse;
import pl.gamilife.user.dto.service.GetUsersCommand;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.specification.UserSpecificationBuilder;
import pl.gamilife.user.usecase.GetUsersUseCase;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetUsersUseCaseImpl implements GetUsersUseCase {

    private final UserRepository userRepository;
    private final UserSpecificationBuilder specificationBuilder;

    @Override
    public GetUsersResult execute(GetUsersCommand cmd) {
        Page<User> userPage = userRepository.findAll(
                specificationBuilder.buildSpecification(cmd.username()),
                PageRequest.of(cmd.page(), cmd.size(), Sort.by("username").ascending())
        );

        return new GetUsersResult(
                userPage.getTotalElements(),
                userPage.getTotalPages(),
                userPage.getNumber(),
                userPage.getSize(),
                userPage.getContent()
                        .stream()
                        .map(UserDetailsResponse::from)
                        .toList()
        );
    }
}
