package edu.pjwstk.groups.usecase.deletegroup;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;

    @Override
    public Void execute(DeleteGroupCommand cmd) {
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
