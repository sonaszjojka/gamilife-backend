package pl.gamilife.group.usecase.creategroup;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.group.exception.domain.GroupTypeNotFoundException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.GroupType;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.repository.GroupTypeJpaRepository;
import pl.gamilife.shared.kernel.event.GroupCreatedEvent;

import java.time.ZoneId;
import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final UserApi userApi;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public CreateGroupResult execute(CreateGroupCommand cmd) {
        GroupType groupType = getGroupType(cmd.groupTypeId());

        ZoneId zoneId = cmd.zoneId() == null
                ? userApi.getUserZoneId(cmd.userId())
                : cmd.zoneId();
        Group group = createGroup(cmd, groupType, cmd.userId(), zoneId);

        addGroupAdmin(group, cmd.userId());

        eventPublisher.publishEvent(new GroupCreatedEvent(group.getId(), group.getName()));

        return buildCreateGroupResult(group);
    }

    private GroupType getGroupType(Integer groupTypeId) {
        return groupTypeRepository.findById(groupTypeId)
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        groupTypeId + " not found!"));
    }

    private Group createGroup(CreateGroupCommand cmd, GroupType groupType, UUID adminUserId, ZoneId zoneId) {
        Group group = Group.create(
                cmd.groupName(),
                adminUserId,
                cmd.groupCurrencySymbol(),
                cmd.membersLimit(),
                zoneId,
                groupType
        );

        return groupRepository.save(group);
    }

    private void addGroupAdmin(Group group, UUID adminUserId) {
        GroupMember groupMemberAdmin = GroupMember.create(group, adminUserId);
        groupMemberRepository.save(groupMemberAdmin);
    }

    private CreateGroupResult buildCreateGroupResult(Group group) {
        return CreateGroupResult.builder()
                .groupId(group.getId())
                .groupName(group.getName())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(new CreateGroupResult.GroupTypeDto(
                        group.getType().getId(),
                        group.getType().getTitle()
                ))
                .build();
    }
}
