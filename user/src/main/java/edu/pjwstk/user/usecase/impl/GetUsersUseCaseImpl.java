package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.response.GetUsersResult;
import edu.pjwstk.user.dto.response.UserDetailsResponse;
import edu.pjwstk.user.dto.service.GetUsersCommand;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.specification.UserSpecificationBuilder;
import edu.pjwstk.user.usecase.GetUsersUseCase;
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
