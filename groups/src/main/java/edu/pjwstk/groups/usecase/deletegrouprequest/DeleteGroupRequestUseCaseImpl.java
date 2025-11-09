package edu.pjwstk.groups.usecase.deletegrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.exception.domain.GroupRequestNotFoundException;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class DeleteGroupRequestUseCaseImpl implements DeleteGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final AuthApi authApi;

    public DeleteGroupRequestUseCaseImpl(GroupRequestJpaRepository groupRequestRepository, AuthApi authApi) {
        this.groupRequestRepository = groupRequestRepository;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public void execute(UUID groupRequestId) {
        GroupRequest groupRequest = groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!Objects.equals(currentUserDto.userId(), groupRequest.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only user who created group request can delete group request!");
        }

        groupRequestRepository.deleteById(groupRequestId);
    }
}
