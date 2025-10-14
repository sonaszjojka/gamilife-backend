package edu.pjwstk.groups.usecase.updategroup;

import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.GroupTypeNotFoundException;
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

    public UpdateGroupUseCaseImpl(GroupRepository groupRepository, GroupTypeRepository groupTypeRepository, UserApi userApi, UpdateGroupMapper updateGroupMapper) {
        this.groupRepository = groupRepository;
        this.groupTypeRepository = groupTypeRepository;
        this.userApi = userApi;
        this.updateGroupMapper = updateGroupMapper;
    }

    @Override
    @Transactional
    public UpdateGroupResponse execute(UpdateGroupRequest request, UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));

        GroupType groupType = groupTypeRepository.findById(request.groupTypeId())
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " + request.groupTypeId() + " not found!"));

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(request.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + request.adminId() + " not found!");
        }

        group.setGroupCurrencySymbol(request.groupCurrencySymbol());
        group.setMembersLimit(request.membersLimit());
        group.setAdminId(admin.get().userId());
        group.setGroupType(groupType);

        Group savedGroup = groupRepository.save(group);
        return updateGroupMapper.toResponse(savedGroup);
    }
}
