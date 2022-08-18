# [The Zen of Python](https://peps.python.org/pep-0020/) and Loyalty-Kr
Zen of Python은 Python 프로그래밍 언어의 설계에 영향을 주는 19개의 격언 모음입니다.

Loyalty-Kr is an application built as independent *Java* software components deployed across several computing instances that fulfill the goal of serving rewards to PC방 users in Korea and influencers across the globe.

## Beautiful is better than ugly.
아름다움은 보는 사람의 눈에 달려 있다. 

프로그래밍 언어에 있어서, 개발자는 해당 언어의 programming idiom을 사용해야 합니다.

Java에서 Python type conversion idiom 쓰는예:
```diff
     private boolean entitlementsContains(String instanceId, EntitlementSetDTO_v3 data) {
-        for (EntitlementDTO_v3 item : data) {
-            if (item.getItem().getId().toString().equals(instanceId)) {
-                return true;
-            }
-        }
-        return false;
+        Set<String> set = data.stream().map(
+                entitlement -> entitlement.getItem().getId().toString()
+        ).collect(Collectors.toSet());
+        return set.contains(instanceId);
     }
```
복잡도는 동일하지만 (또는 더 나쁠수도) 두 번째가 훨씬 더 읽기 쉽습니다.

## Explicit (명시적) is better than implicit (암시적).
함수의 동작에 대해 클라이언트에게 명시적이어야 합니다.
```diff
-    public Mono<Void> revokeCapEntitlement(PlayerActivityEvent playerActivityEvent) {
+    public Mono<Void> revokeCapEntitlementIfRequired(PlayerActivityEvent playerActivityEvent) {
        if (playerActivityEvent.isStartOrEnd()){playerActivityEvent.revokeReward();}
        }
```

또는 revocation의 조건을 나타낼수도 있습니다.
```diff
-    public Mono<Void> revokeCapEntitlementIfRequired(PlayerActivityEvent playerActivityEvent) {
+    public Mono<Void> revokeCapEntitlementIfStartOrEndEvent(PlayerActivityEvent playerActivityEvent) {
        if (playerActivityEvent.isStartOrEnd()){playerActivityEvent.revokeReward();}
        }
```

## Simple is better than complex. Complex is better than complicated.
여러 가지 간단화할 수 있는 방법이 있는데, 저한태는 가장 쉬운 방법은 third party library들을 잘 알고 사용하는것이다:
```diff
 private RewardEvent translateToRewardEvent(PlayerActivityEvent playerActivityEvent, boolean isGrant) {
-        return RewardEvent.builder()
-                .rewardIds(playerActivityEvent.getRewardIds())
-                .gameType(playerActivityEvent.getGameType())
-                .puuid(playerActivityEvent.getPuuid())
-                .sessionToken(playerActivityEvent.getSessionToken())
-                .isGrant(isGrant)
-                .traceId(playerActivityEvent.getTraceId())
-                .accountId(playerActivityEvent.getAccountId())
-                .gameShard(playerActivityEvent.getGameShard())
-                .build();
+        RewardEvent RewardEvent = RewardEvent.builder().build();
+        BeanUtils.copyProperties(playerActivityEvent, RewardEvent);
+        RewardEvent.setGrant(isGrant);
+        return RewardEvent;
     }
```

## Flat is better than nested.
Java는 이 격언과 근본적으로 모순됩니다.

Modules are highly nested:
```java
import com.riotgames.loyalty.admin.repository.LoyaltyRewardRepository;
import com.riotgames.loyalty.admin.service.catalog.CatalogService;
import com.riotgames.loyalty.admin.service.reward.LoyaltyRewardPublisher;
import com.riotgames.loyalty.admin.service.reward.RewardBuilderService;
import com.riotgames.loyalty.admin.service.reward.rewardable.LoyaltyRewardableProvider;
```

심시어 원래 혼자서 존재할 수 있는 함수들은 class에 붙어 있어야 됩니다.
```java
public class RewardUtil {
    public static RewardItem createChampion(String championId, boolean activated) {
        return RewardItem.builder()
                .activated(activated)
                .inventoryType(CapUuid.LOL_CHAMPION_UUID)
                .id(championId)
                .defaultLocked(false)
                .name("")
                .build();
    }
```

여전히 적용할 수 있는 곳이 있습니다 (또는 "Explicit (명시적) is better than implicit (암시적)")
```java
public Mono<Void> revokeCapLoyaltyEntitlementIfRequired(PlayerActivityEvent playerActivityEvent) {
-        if (isEligibleForRevoke(playerActivityEvent)) {
+        if (playerActivityEvent.isApplicationStartPacket() || playerActivityEvent.isApplicationEndPacket()) {
            return rewardNotifierApiClient.revokeCapEntitlement(playerActivityEvent)
```
하지만 revocation 조건을 숨겨서 좋을수도 있습니다.

## Sparse is better than dense.
[단일 책임 원칙](https://ko.wikipedia.org/wiki/%EB%8B%A8%EC%9D%BC_%EC%B1%85%EC%9E%84_%EC%9B%90%EC%B9%99) 비슷하게도, 각 코드 블록은 꽤 작은 작업을 담당해야 합니다. 

이것은 반드시 "flat is better than nested"랑 모순될 필요는 없다. 

긴 함수를 여러 작은 코드 블록으로 분리할수 있습니다:
```java
void updateLoyaltyRewardsForCapEntitlements(LoyaltyReward loyaltyReward) {
    var gameShard = loyaltyReward.getGameShard();
    var gameType = loyaltyReward.getGameType();
    var productId = getProductId(loyaltyReward);
    var capTypeId = loyaltyReward.getCapLoyaltyTypeId();

    List<RewardItem> rewardItemsToActivate = loyaltyReward.getRewardItems().stream()
            .filter(RewardItem::isActivated)
            .collect(Collectors.toList());

    // search for all entitlements
    EntitlementSetDTO_v3 entitlementsBeforeUpdate = entitlementsApiClient.searchEntitlements(
                    gameShard, gameType, productId, capTypeId, null, CapUuid.LOYALTY_PLUGIN_TYPE_ID, null, null
            ).getData().stream()
            // Cap returns entitlements that don't belong to the ownerId despite the query parameter
            .filter(entitlement -> entitlement.getOwnerId() != null && entitlement.getOwnerId().toString().equals(capTypeId))
            .collect(Collectors.toCollection(EntitlementSetDTO_v3::new));
    log.info("There are {} active/inactive entitlements before the update.", entitlementsBeforeUpdate.size());

    // deactivate previously active entitlements that are to be deactivated
    Set<String> rewardItemIdsToActivate = rewardItemsToActivate.stream()
            .map(RewardItem::getId)
            .collect(Collectors.toSet());
    EntitlementDeactivateListDTO_v3 entitlementsToDeactivate = EntitlementDeactivateListDTO_v3.builder().deactivatedIds(
                    entitlementsBeforeUpdate.stream()
                            .filter(EntitlementDTO_v3::getActive)
                            .filter(entitlement -> !rewardItemIdsToActivate.contains(entitlement.getItemId().toString()))
                            .map(EntitlementDTO_v3::getId)
                            .collect(Collectors.toList())
            ).build();
    if (entitlementsToDeactivate.getDeactivatedIds().isEmpty()) {
        log.info("There are no entitlements items to deactivate. No delete call will be made.");
    } else {
        log.info(
                "{} entitlements were deactivated.",
                entitlementsApiClient.deactivateEntitlements(gameShard, gameType, productId, capTypeId, buildRequestDTO_v2(entitlementsToDeactivate)).getData().size()
        );
    }

    // activate previously inactive entitlements that are to be activated
    EntitlementListDTO_v3 entitlementsToActivate = entitlementsBeforeUpdate.stream()
            .filter(entitlement -> !entitlement.getActive())
            .filter(entitlement -> rewardItemIdsToActivate.contains(entitlement.getItemId().toString()))
            .map(entitlement -> {
                entitlement.setLabel(LOYALTY_REWARD);
                entitlement.setActive(true);
                return entitlement;
            })
            .collect(Collectors.toCollection(EntitlementListDTO_v3::new));
    if (entitlementsToActivate.isEmpty()) {
        log.info("There are no entitlements items to activate. No update call will be made.");
    } else {
        log.info("{} pre-existing entitlements were activated.",
                entitlementsApiClient.updateEntitlements(gameShard, gameType, productId, capTypeId, buildRequestDTO_v2(entitlementsToActivate)).getData().size()
        );
    }

    // create leftover entitlements that did not exist
    Set<String> existingEntitlementItemIds = entitlementsBeforeUpdate.stream().map(entitlement -> entitlement.getItemId().toString()).collect(Collectors.toSet());
    EntitlementListDTO_v3 entitlementsToCreate = rewardItemsToActivate.stream()
            // only create entitlements that did not exist previously
            .filter(rewardItem -> !existingEntitlementItemIds.contains(rewardItem.getId()))
            .map(rewardItem ->
                    EntitlementDTO_v3.builder()
                            .productId(UUID.fromString(productId))
                            .label(LOYALTY_REWARD)
                            .typeId(UUID.fromString(CapUuid.LOYALTY_PLUGIN_TYPE_ID))
                            .ownerId(UUID.fromString(capTypeId))
                            .item(rewardItem.toItemDTO_v3())
                            .build())
            .collect(Collectors.toCollection(EntitlementListDTO_v3::new));
    if (entitlementsToCreate.isEmpty()) {
        log.info("There are no entitlements to create. No create call will be made.");
    } else {
        log.info("{} new entitlements were created.",
                entitlementsApiClient.createEntitlements(gameShard, gameType, productId, capTypeId, buildRequestDTO_v2(entitlementsToCreate)).getData().size()
        );
    }

    entitlementsAdminApiClient.refreshLoyaltyCapEntitlements(gameShard, gameType);
}
```

## Readability counts.
이 격언은 모든 격언의 포괄적인 우산 격언입니다.

## Special cases aren't special enough to break the rules.
## Although practicality beats purity.

## Errors should never pass silently.
Python에서, checked exception은 조재하지 않아 모든 exception은 unchecked exception입니다 (`RuntimeException`, `Error`, and their subclasses). 

Effective Java 제69조에 관하여, exception은 예외적인 경우에서만 쓰여야 되니까 침묵 시켜져서 넘으면 안됩니다!

제77조에 하지마라는예:
```java
try {
...
} catch (SomeException e) {
}
```

## Unless explicitly silenced.
Exception을 무시하려면, catch 블록에는 왜 그렇게 하는 것을 설명하는 comment가 포함되어야 하고 exception variable 이름은 `ignored`이어야 합니다.
제77조에 좋은예:
```java
Future<Integer> favouriteChampionId = exec.submit(myBadLolStatsApi::getFavouriteChampionId);
int championId = 1; // Default champion is Annie
try {
championId = f.get(1L, TimeUnit.SECONDS);
} catch (NotFoundException  ignored) {
// If the player doesn't have a favourite champion, just provide the default Annie
}
```

## In the face of ambiguity, refuse the temptation to guess.

## There should be one-- and preferably only one --obvious way to do it. Although that way may not be obvious at first unless you're Dutch.
This is a reference to the **Dutch** created of Python [Guido van Rossum](https://en.wikipedia.org/wiki/Guido_van_Rossum), because for him, all of the solutions in Python are obvious for obvious reasons.

장난말고, 이 개념은 두번 생각해보라는말입니다.

[CAP migration 예](https://gh.riotgames.com/loyalty-kr/loyalty-kr-python-scripts/pull/12):
```diff
pcBang_collection.update_many(
     filter={"_id": {"$in": pcbs_to_migrate["_id"].tolist()}},
-    update={"$pull": {"loyaltyRewards": row.legacyReward}},
+    update={"$pull": {"loyaltyRewards": {"rewardId": row.legacyRewardId}}},
)
```


## Now is better than never.
최적화 할수 있는 아이디어가 있다면 지금 하십시오.

Example from `loyalty-admin-be`:
```java
public class RewardItem {
    // other code
    public ItemDTO_v3 toItemDTO_v3() {
        return ItemDTO_v3.builder()
                .id(getUuid())
                .typeId(UUID.fromString(this.getInventoryType()))
                .build();
    }
```

```java
EntitlementDTO_v3.builder()
    .productId(UUID.fromString(productId))
    .label(LOYALTY_REWARD)
    .typeId(UUID.fromString(CapUuid.LOYALTY_PLUGIN_TYPE_ID))
    .ownerId(UUID.fromString(capTypeId))
    .item(rewardItem.toItemDTO_v3())
    .build())
```

## Although never is often better than *right* now.
그럼에도 불고 하고 결코 안한는것은 굳이 지금 당장 하는것보다는 나을수도 있습니다. 

지나치게 구축하지 마세요.[YAGNI (You aren't gonna need it) 원칙](https://ko.wikipedia.org/wiki/YAGNI)을 유의하세요.

위에 예에 관하여, `RewardItem`으로부터 또 다른 class으로 옮기는 method가 아직 필요 없잖아요 (`toEntitlementDTO_v3`, `toEntitlementListDTO_v3`, etc.).

## If the implementation is hard to explain, it's a bad idea.
설명하기 더 쉬운 부분들로 리팩토링하십시오.

## If the implementation is easy to explain, it may be a good idea.
하지만 쉽게 설명할수 있다고 반드시 좋은 아이디어가 아닙니다.

## Namespaces are one honking great idea -- let's do more of those!
네임스페이스 사용하는것은 변수를 구성하는 좋은 방법입니다. 

Java에서 namespace 덕분에 똑같은 이름의 constant들을 가진 Enum들은 서로 평화하게 존해 할수 있습니다. 

`loyalty-admin-be` 예:
```java
public enum GameType {LOL, VALORANT}
public enum RewardType {LEGACY_LOL, LOL, TFT, VALORANT}
```
이로써 `LOL` `GameType`이 `LOL` `RewardType`이랑 다르게 다룰수 있습니다.
