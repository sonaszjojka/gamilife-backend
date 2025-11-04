package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.GroupTypeNotFoundException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupTypeRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class UpdateGroupUseCaseImpl implements UpdateGroupUseCase {

    private final GroupRepository groupRepository;
    private final GroupTypeRepository groupTypeRepository;
    private final UserApi userApi;
    private final UpdateGroupMapper updateGroupMapper;
    private final AuthApi authApi;

    public UpdateGroupUseCaseImpl(GroupRepository groupRepository, GroupTypeRepository groupTypeRepository, UserApi userApi, UpdateGroupMapper updateGroupMapper, AuthApi authApi) {
        this.groupRepository = groupRepository;
        this.groupTypeRepository = groupTypeRepository;
        this.userApi = userApi;
        this.updateGroupMapper = updateGroupMapper;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public UpdateGroupResponse execute(UpdateGroupRequest request, UUID groupId) {
        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));

        if (currentUserDto.userId() != group.getAdminId()) {
            throw new UserNotGroupAdministratorAccessDeniedException("Only group administrators can edit group!");
        }

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(request.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + request.adminId() + " not found!");
        }

        GroupType groupType = groupTypeRepository.findById(request.groupType().getId())
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        request.groupType().getId() + " not found!"));

        group.setGroupCurrencySymbol(request.groupCurrencySymbol());
        group.setMembersLimit(request.membersLimit());
        group.setAdminId(admin.get().userId());
        group.setGroupType(groupType);
        group.setName(request.groupName());

        Group savedGroup = groupRepository.save(group);
        return updateGroupMapper.toResponse(savedGroup);
    }
}
