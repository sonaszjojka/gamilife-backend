package edu.pjwstk.groups.usecase.deletegroup;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;

    public DeleteGroupUseCaseImpl(GroupJpaRepository groupRepository, AuthApi authApi) {
        this.groupRepository = groupRepository;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public void execute(UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!Objects.equals(currentUserDto.userId(), group.getAdminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete groups!");
        }

        groupRepository.deleteById(groupId);
    }
}
