package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
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
@AllArgsConstructor
public class EditGroupUseCaseImpl implements EditGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final UserApi userApi;
    private final AuthApi authApi;

    @Override
    @Transactional
    public EditGroupResult executeInternal(EditGroupCommand cmd) {
        CurrentUserDto currentUserDto = authApi.getCurrentUser();
        Group group = getGroup(cmd.groupId());

        if (group.isUserAdmin(currentUserDto.userId())) {
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
        EditGroupResult.GroupTypeDto groupTypeDto = group.getGroupType() != null ?
                new EditGroupResult.GroupTypeDto(
                        group.getGroupType().getGroupTypeId(),
                        group.getGroupType().getTitle()
                ) : null;

        return EditGroupResult.builder()
                .groupId(group.getGroupId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getGroupCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(groupTypeDto)
                .build();
    }
}
