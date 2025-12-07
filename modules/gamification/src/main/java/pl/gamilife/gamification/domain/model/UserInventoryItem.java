package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import lombok.experimental.SuperBuilder;
import pl.gamilife.gamification.domain.exception.UserDoesNotHaveEnoughItemsException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.util.UUID;

@Getter
@Entity
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(exclude = {"item"})
@Table(name = "user_inventory_item")
public class UserInventoryItem extends BaseEntity {

    @Column(name = "item_id", nullable = false, insertable = false, updatable = false)
    private UUID itemId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "item_id", nullable = false)
    private Item item;

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @NotNull
    @Column(name = "is_equipped", nullable = false)
    @Builder.Default
    private Boolean isEquipped = false;

    public boolean doesBelongTo(UUID userId) {
        return this.userId.equals(userId);
    }

    public void incrementQuantityBy(int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Quantity increment be greater than 0");
        }

        this.quantity += amount;
    }

    public int quickSellItems(Integer amount) {
        if (amount > quantity) {
            throw new UserDoesNotHaveEnoughItemsException(
                    String.format("User has only %s such items in inventory", quantity)
            );
        }

        quantity -= amount;

        return item.getQuickSellValue() * amount;
    }

    public boolean changeEquippedStatus(boolean newStatus) {
        if (isEquipped == newStatus) {
            return false;
        }

        this.isEquipped = newStatus;
        return true;
    }
}