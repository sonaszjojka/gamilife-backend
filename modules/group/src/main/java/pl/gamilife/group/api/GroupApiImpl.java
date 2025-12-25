package pl.gamilife.group.api;

import org.springframework.stereotype.Service;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.group.usecase.editmemberwallet.EditMemberWalletUseCase;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdCommand;
import pl.gamilife.group.usecase.findgroupbyid.FindGroupByIdUseCase;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdCommand;
import pl.gamilife.group.usecase.findgroupmemberbyid.FindGroupMemberByIdUseCase;

import java.util.UUID;

@Service
public class GroupApiImpl implements GroupApi {

    private final FindGroupMemberByIdUseCase findGroupMemberByIdUseCase;
    private final FindGroupByIdUseCase findGroupByIdUseCase;
    private final EditMemberWalletUseCase editMemberWalletUseCase;

    public GroupApiImpl(FindGroupMemberByIdUseCase findGroupMemberByIdUseCase,
                        FindGroupByIdUseCase findGroupByIdUseCase, EditMemberWalletUseCase editMemberWalletUseCase) {
        this.findGroupMemberByIdUseCase = findGroupMemberByIdUseCase;
        this.findGroupByIdUseCase = findGroupByIdUseCase;
        this.editMemberWalletUseCase = editMemberWalletUseCase;
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
