package pl.gamilife.group.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.BasicGroupMemberDto;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdCommand;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdUseCase;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdCommand;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;
import pl.gamilife.group.usecase.findgroupmemberbyuserid.FindGroupMemberByUserIdCommand;
import pl.gamilife.group.usecase.findgroupmemberbyuserid.FindGroupMemberByUserIdUseCase;
import pl.gamilife.group.usecase.findmembersbyidin.FindMembersByIdInCommand;
import pl.gamilife.group.usecase.findmembersbyidin.FindMembersByIdInUseCase;
import pl.gamilife.group.usecase.getgrouptimezone.GetGroupTimezoneCommand;
import pl.gamilife.group.usecase.getgrouptimezone.GetGroupTimezoneUseCase;
import pl.gamilife.group.usecase.grantrewardstomembers.GrantRewardsToMembersCommand;
import pl.gamilife.group.usecase.grantrewardstomembers.GrantRewardsToMembersUseCase;
import pl.gamilife.group.usecase.makepayment.MakePaymentCommand;
import pl.gamilife.group.usecase.makepayment.MakePaymentUseCase;

import java.time.ZoneId;
import java.util.Collection;
import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;
    private final FindGroupByIdUseCase findGroupByIdUseCase;
    private final GrantRewardsToMembersUseCase grantRewardsToMembersUseCase;
    private final GetGroupTimezoneUseCase getGroupTimezoneUseCase;
    private final FindGroupMemberByUserIdUseCase findGroupMemberByUserIdUseCase;
    private final MakePaymentUseCase makePaymentUseCase;
    private final FindMembersByIdInUseCase findMembersByIdInUseCase;

    @Override
    public ZoneId getGroupTimezone(UUID groupId) {
        return getGroupTimezoneUseCase.execute(new GetGroupTimezoneCommand(groupId));
    }

    @Override
    public GroupMemberDto findGroupMemberById(UUID groupId, UUID groupMemberId) {
        return findGroupMemberByIdUseCase.execute(
                new FindGroupMemberByIdCommand(groupId, groupMemberId)
        );
    }

    @Override
    public GroupDto findGroupById(UUID groupId) {
        return findGroupByIdUseCase.execute(new FindGroupByIdCommand(groupId));
    }

    @Override
    public Collection<BasicGroupMemberDto> grantRewards(Collection<UUID> groupMemberIds, Integer amount) {
        var result = grantRewardsToMembersUseCase.execute(new GrantRewardsToMembersCommand(
                groupMemberIds,
                amount
        ));

        return result.stream()
                .map(gm -> new BasicGroupMemberDto(
                                gm.groupMemberId(),
                                gm.groupMoney(),
                                gm.totalEarnedMoney(),
                                gm.userId()
                        )
                ).toList();
    }

    @Override
    public Collection<BasicGroupMemberDto> getBasisGroupMemberDtoByIdIn(Collection<UUID> groupMemberIds) {
        return findMembersByIdInUseCase.execute(new FindMembersByIdInCommand(groupMemberIds)).stream()
                .map(r -> new BasicGroupMemberDto(
                        r.groupMemberId(),
                        r.groupMoney(),
                        r.totalEarnedMoney(),
                        r.userId()
                )).toList();
    }

    @Override
    public Optional<GroupMemberDto> findGroupMemberByUserId(UUID userId, UUID groupId) {
        var result = findGroupMemberByUserIdUseCase.execute(new FindGroupMemberByUserIdCommand(userId, groupId));
        return result.map(gm -> new GroupMemberDto(
                gm.groupMemberId(),
                new GroupMemberDto.GroupDto(gm.groupId(), gm.groupName()),
                gm.userId(),
                gm.joinedAt(),
                gm.leftAt(),
                gm.groupMoney(),
                gm.totalEarnedMoney(),
                gm.isAdmin()
        ));
    }

    @Override
    public void payForItem(UUID groupMemberId, Integer price) {
        makePaymentUseCase.execute(new MakePaymentCommand(groupMemberId, price));
    }

}
