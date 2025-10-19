package edu.pjwstk.groups.usecase.deletegrouprequest;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.exception.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.exception.UserNotOwnerAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.jpa.GroupRequestRepositoryJpa;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class DeleteGroupRequestUseCaseImpl implements DeleteGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;
    private final AuthApi authApi;

    public DeleteGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository, AuthApi authApi) {
        this.groupRequestRepository = groupRequestRepository;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public void execute(UUID groupRequestId) {
        GroupRequest groupRequest = groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (!Objects.equals(currentUserDto.userId(), groupRequest.getUserId())) {
            throw new UserNotOwnerAccessDeniedException("Only user who created group request can delete group request!");
        }

        groupRequestRepository.deleteById(groupRequestId);
    }
}
