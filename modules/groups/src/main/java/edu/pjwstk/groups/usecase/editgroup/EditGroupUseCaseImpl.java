package edu.pjwstk.groups.usecase.editgroup;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupType;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupTypeJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupUseCaseImpl implements EditGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final UserApi userApi;
    private final AuthApi authApi;

    @Override
    public EditGroupResult execute(EditGroupCommand cmd) {
        CurrentUserDto currentUserDto = authApi.getCurrentUser();
        Group group = getGroup(cmd.groupId());

        if (!group.isUserAdmin(currentUserDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group!");
        }

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(cmd.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + cmd.adminId() + " not found!");
        }

        GroupType groupType = getGroupType(cmd.groupTypeId());

        group.setGroupCurrencySymbol(cmd.groupCurrencySymbol());
        group.setMembersLimit(cmd.membersLimit());
        group.setAdminId(admin.get().userId());
        group.setGroupType(groupType);
        group.setName(cmd.groupName());
        Group savedGroup = groupRepository.save(group);

        return buildEditGroupResult(savedGroup);
    }

    private GroupType getGroupType(Integer groupTypeId) {
        return groupTypeRepository.findById(groupTypeId)
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        groupTypeId + " not found!"));
    }

    private Group getGroup(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));
    }

    private EditGroupResult buildEditGroupResult(Group group) {
        return EditGroupResult.builder()
                .groupId(group.getGroupId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getGroupCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(new EditGroupResult.GroupTypeDto(
                        group.getGroupType().getGroupTypeId(),
                        group.getGroupType().getTitle()
                ))
                .build();
    }
}
