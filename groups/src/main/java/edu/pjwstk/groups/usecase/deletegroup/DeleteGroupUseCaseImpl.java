package edu.pjwstk.groups.usecase.deletegroup;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;

    @Override
    @Transactional
    public Void executeInternal(DeleteGroupCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId())
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!group.isUserAdmin(currentUserDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete groups!");
        }

        groupRepository.deleteById(cmd.groupId());

        return null;
    }
}
