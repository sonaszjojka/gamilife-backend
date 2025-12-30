package pl.gamilife.group.usecase.deletegrouprequest;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.exception.domain.GroupRequestNotFoundException;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.repository.GroupRequestJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupRequestUseCaseImpl implements DeleteGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;

    @Override
    public Void execute(DeleteGroupRequestCommand cmd) {
        GroupRequest groupRequest = getGroupRequest(cmd.groupId(), cmd.groupRequestId());

        if (!groupRequest.belongsToUser(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only user who created group request can delete group request!");
        }

        groupRequestRepository.deleteById(cmd.groupRequestId());

        return null;
    }

    private GroupRequest getGroupRequest(UUID groupId, UUID groupRequestId) {
        return groupRequestRepository.findByIdAndGroupId(groupRequestId, groupId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));
    }
}
