package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupShopStatusResult;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupShopStatusUseCase;
import pl.gamilife.groupshop.application.changegroupshopstatus.ChangeGroupStatusCommand;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopCommand;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopResult;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopUseCase;
import pl.gamilife.groupshop.application.getgroupshopdetails.GetGroupShopDetailsCommand;
import pl.gamilife.groupshop.application.getgroupshopdetails.GetGroupShopDetailsResult;
import pl.gamilife.groupshop.application.getgroupshopdetails.GetGroupShopDetailsUseCase;
import pl.gamilife.groupshop.application.getgroupshopitems.GetGroupShopItemsCommand;
import pl.gamilife.groupshop.application.getgroupshopitems.GetGroupShopItemsResult;
import pl.gamilife.groupshop.application.getgroupshopitems.GetGroupShopItemsUseCase;
import pl.gamilife.groupshop.infrastructure.web.request.ChangeGroupShopStatusRequest;
import pl.gamilife.groupshop.infrastructure.web.request.EditGroupShopRequest;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {

    private final ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase;
    private final EditGroupShopUseCase editGroupShopUseCase;
    private final GetGroupShopDetailsUseCase getGroupShopDetailsUseCase;
    private final GetGroupShopItemsUseCase getGroupShopItemsUseCase;

    @PutMapping
    public ResponseEntity<EditGroupShopResult> editGroupShop(@PathVariable UUID groupId,
                                                             @CurrentUserId UUID currentUserId,
                                                             @RequestBody EditGroupShopRequest request) {
        EditGroupShopCommand cmd = new EditGroupShopCommand(request.name(), request.description(), groupId, currentUserId);

        EditGroupShopResult response = editGroupShopUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @PatchMapping
    public ResponseEntity<ChangeGroupShopStatusResult> changeGroupShopStatus(@PathVariable UUID groupId,
                                                                             @CurrentUserId UUID currentUserId,
                                                                             @RequestBody @Valid ChangeGroupShopStatusRequest request) {
        ChangeGroupStatusCommand cmd = new ChangeGroupStatusCommand(request.isActive(), groupId, currentUserId);
        ChangeGroupShopStatusResult response = changeGroupShopStatusUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<GetGroupShopDetailsResult> getGroupItemsInShop(@PathVariable UUID groupId,
                                                                         @CurrentUserId UUID userId,
                                                                         @RequestParam(name = "page", defaultValue = "0") @Min(0) int page,
                                                                         @RequestParam(name = "size", defaultValue = "10") @Min(0) @Max(100) int size) {
        var response = getGroupShopDetailsUseCase.execute(new GetGroupShopDetailsCommand(groupId, userId, page, size));
        return ResponseEntity.ok(response);
    }

    @GetMapping("/items")
    public ResponseEntity<Page<GetGroupShopItemsResult>> getGroupItemsInShop(
            @PathVariable UUID groupId,
            @CurrentUserId UUID userId,
            @RequestParam(name = "isActive", required = false) Boolean isActive,
            @RequestParam(name = "page", defaultValue = "0") @Min(0) int page,
            @RequestParam(name = "size", defaultValue = "10") @Min(0) @Max(100) int size
    ) {
        var response = getGroupShopItemsUseCase.execute(new GetGroupShopItemsCommand(
                groupId,
                userId,
                isActive,
                page,
                size)
        );
        return ResponseEntity.ok(response);
    }
}
