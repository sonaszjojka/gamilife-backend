package pl.gamilife.group.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.group.usecase.editmemberwallet.EditMemberWalletUseCase;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdCommand;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdUseCase;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdCommand;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;
import pl.gamilife.group.usecase.getgrouptimezone.GetGroupTimezoneCommand;
import pl.gamilife.group.usecase.getgrouptimezone.GetGroupTimezoneUseCase;

import java.time.ZoneId;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;
    private final FindGroupByIdUseCase findGroupByIdUseCase;
    private final EditMemberWalletUseCase editMemberWalletUseCase;
    private final GetGroupTimezoneUseCase getGroupTimezoneUseCase;

    @Override
    public ZoneId getGroupTimezone(UUID groupId) {
        return getGroupTimezoneUseCase.execute(new GetGroupTimezoneCommand(groupId));
    }

    @Override
    public GroupMemberDto findGroupMemberById(UUID groupMemberId) {
        return findGroupMemberByIdUseCase.execute(
                new FindGroupMemberByIdCommand(groupMemberId)
        );
    }

    @Override
    public GroupDto findGroupById(UUID groupId) {
        return findGroupByIdUseCase.execute(new FindGroupByIdCommand(groupId));
    }

    @Override
    public void editMemberWallet(UUID memberId, UUID groupId, Integer amount) {

        editMemberWalletUseCase.execute(groupId, memberId, amount);
    }


}
