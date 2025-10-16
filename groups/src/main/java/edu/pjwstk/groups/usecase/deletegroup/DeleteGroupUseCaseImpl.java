package edu.pjwstk.groups.usecase.deletegroup;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private final GroupRepository groupRepository;
    private final AuthApi authApi;

    public DeleteGroupUseCaseImpl(GroupRepository groupRepository, AuthApi authApi) {
        this.groupRepository = groupRepository;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public void execute(UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (!Objects.equals(currentUserDto.userId(), group.getAdminId())) {
            throw new UserNotGroupAdministratorAccessDeniedException("Only group administrators can delete groups!");
        }

        groupRepository.deleteById(groupId);
    }
}
