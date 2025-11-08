package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupTypeJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class UpdateGroupUseCaseImpl implements UpdateGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final UserApi userApi;
    private final UpdateGroupMapper updateGroupMapper;
    private final AuthApi authApi;

    public UpdateGroupUseCaseImpl(GroupJpaRepository groupRepository, GroupTypeJpaRepository groupTypeRepository, UserApi userApi, UpdateGroupMapper updateGroupMapper, AuthApi authApi) {
        this.groupRepository = groupRepository;
        this.groupTypeRepository = groupTypeRepository;
        this.userApi = userApi;
        this.updateGroupMapper = updateGroupMapper;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public UpdateGroupResponse execute(UpdateGroupRequest request, UUID groupId) {
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));

        if (currentUserDto.userId() != group.getAdminId()) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group!");
        }

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(request.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + request.adminId() + " not found!");
        }

        GroupType groupType = groupTypeRepository.findById(request.groupTypeId())
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        request.groupTypeId() + " not found!"));

        group.setGroupCurrencySymbol(request.groupCurrencySymbol());
        group.setMembersLimit(request.membersLimit());
        group.setAdminId(admin.get().userId());
        group.setGroupType(groupType);
        group.setName(request.groupName());

        Group savedGroup = groupRepository.save(group);
        return updateGroupMapper.toResponse(savedGroup);
    }
}
