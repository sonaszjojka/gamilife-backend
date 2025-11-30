package edu.pjwstk.groups.usecase.deletegrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.exception.domain.GroupRequestNotFoundException;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupRequestUseCaseImpl implements DeleteGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final AuthApi authApi;

    @Override
    public Void execute(DeleteGroupRequestCommand cmd) {
        GroupRequest groupRequest = getGroupRequest(cmd.groupId(), cmd.groupRequestId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!groupRequest.belongsToUser(currentUserDto.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only user who created group request can delete group request!");
        }

        groupRequestRepository.deleteById(cmd.groupRequestId());

        return null;
    }

    private GroupRequest getGroupRequest(UUID groupId, UUID groupRequestId) {
        return groupRequestRepository.findByGroupRequestIdAndGroupRequested_GroupId(groupRequestId, groupId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));
    }
}
